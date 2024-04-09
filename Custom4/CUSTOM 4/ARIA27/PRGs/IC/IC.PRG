*B601911,1 AMM 02/12/98 1- Add the function lfOGRShow() to Control enabling 
*B601911,1 AMM          or disabling objects in the setting grid before displaying 
*B601911,1 AMM          2- Add the function lfvChgDye() to Validate the changing 
*B601911,1 AMM          of dyelot setup.
*B601911,1 AMM          3- Add the function lfvWareLoc() to Validate the changing 
*B601911,1 AMM          of WareHouse locations setup.
*B601911,1 AMM          4- Add the function lfvUCCDiv() to Validate the changing 
*B601911,1 AMM          of 'Maintain U.C.C. Manf. ID '.
*B601911,1 AMM          5- Add the function lfvPack() to Validate the changing 
*B601911,1 AMM          of 'USE STYLE PACKS/SKU' setting.
*E301073,1 AMM 11/29/98 Enable or disable the Color assortment code POPUP due to 
*E301073,1 AMM          the Use color assortments setting (Y/N)
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B602281,1 AMM 02/16/99  Validate the setting of extended size scale (Y/N)
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*B602658,5 AMM 03/14/99 Fix the bug of "Syntax error" in IC setups
*E301176,1 HDM 03/21/1999 Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham 03/24/99
*C101459,1 define array laUsrFields to hold user defined fields for the program base file
*B602725,1 AMM 03/31/99 Adjust to fit the new structure of the codes file in 
*B602725,1 AMM          Color Assortment setting
*B802091,1 TAK 05/11/1999 Added style taxeble setup depends on comp.setup yes or no.
*E301209,1 MAB 04/25/99 Add new setup parameters ask use for width of 
*                       extended size scale (Either 1 or 2 Characters).
*E301297,1  HS  07/22/99  Implement the Object/Events triggers, Add a new
*E301297,1                function (gfDoTriger) to handle the Triggers that
*E301297,1                need to be executed when a certain event takes
*E301297,1                place from a certain screen (object).
*B603623,1 AME            If the user make any style cost sheet the
*B603623,1                Setups of the cost elements in ic module setup 
*B603623,1                Will be disabled.     
*B603683,1 AME 06/18/2000 If the user make any style cost sheet the
*B603683,1                Setups of the cost elements in ic module setup 
*B603683,1                Will be disabled But the lables will still Enabled  
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field 
*N119681,1 SSH  10/01/2003 Enabled/Disable style configuration  popup
*N119681,2 WSH  03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
******************************************************************************

PARAMETERS lcProgName,lcPrgPram
lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
  #REGION 1
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
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
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],laPrgTemps[1],;
            laUsrFields[1,7]
  
  *E301297,1 Change this line to add a new row to the array (laCtrStat) [End]

*C101459,1 Hesham (End)
*-HDM E301176,1 03/21/1999[End]

*E301164,1 Hesham (End)

  STORE .F. TO llAddRec     , llEditRec       , llDeleRec      

  STORE  1  TO lnCurObj     , lnBar_No        , lnDataNo        , pbRight        ,;
               pbLeft       , pbButt
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

 * STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
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
               laSubProc    , lcExDifAcc      , lcCompPrnt, laPrgTemps,lcMenUnProce ,;
               lcSydKey     , lcNoteType      , lcNoteKey

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd     ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt, laPrgTemps,lcMenUnProce ,;
               lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

*C101459,1 Hesham (End)
*-- HDM E301176,1 03/21/1999 [End]
  *C101518,1 ASH 04/11/2000 (Begin) Declare filter variables to be used while running report from screen.
  DECLARE   laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]
  STORE '' TO laOGFxFlt,laOGVrFlt, lcReportID   ,laRepFltr
  *C101518,1 ASH 04/11/2000 (End)
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
  *--HDM
  laCtrStat[13] = "DISABLE"                && NotePad  button 
  laCtrStat[14] = "DISABLE"                && User defined fields button 
  
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
  llPrgFound =   (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
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

*B601911,1 AMM Disable the multi or single warehouse setting if it is 'Y' (to privent 
*B601911,1 AMM switching from multi to single warehouse).
=lfOGShowGet('M_WAREHOUS',M_WAREHOUS='N')

*B601911,1 AMM always disable the first cost element in the imported style because 
*B601911,1 AMM it is always 'P. Price'
=lfOGShowGet('M_CITYPE1',.F.)

*E301073,1 AMM Enable or disable the Color assortment code POPUP due to 
*E301073,1 AMM the Use color assortments setting (Y/N)
=lfOGShowGet('MCLRASSCOD',MCLRASSORT = 'Y')
*E301073,1 AMM  end
*B602281,1 AMM If there is any style exist, don't allow to change the setting of extended size scale
*B602658,5 AMM Pass the right parameters to the function
*llOpSty = gfOpenFile(gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'STYLE.DBF','','SH')

*E301209,1 Enable/Disable Extended size scale and its width objects [Begin]
*llOpSty = gfOpenFile(gcDataDir+'STYLE','','SH')
*B602658,5 AMM end
*GO TOP
*=lfOGShowGet('M_USEEXSSC',EOF('STYLE'))

llOpScale = gfOpenFile(gcDataDir+'SCALE','','SH')
GO TOP
=lfOGShowGet('M_USEEXSSC',EOF('SCALE'))
=lfOGShowGet('M_EXTWIDTH',M_USEEXSSC AND EOF('SCALE'))
IF llOpScale
  =gfCloseFile('SCALE')
ENDIF

*IF llOpSty
*  USE IN STYLE
*ENDIF
*E301209,1 Enable/Disable Extended size scale and its width objects [End  ]

*B602281,1 AMM end
*B802091,1 Disable default style taxable setup in company tax setup was 'No'.
=lfOGShowGet('CDEFSTYTAX',gfGetMemVar('M_TAX')='Y')
*B802091,1 End.


*B603623,1 Disable all cost elemanets if there was at least one cost AME[Start]
*B603623,1 sheet generated AME[Start]
llOpBom = gfOpenFile(gcDataDir+'Bom','','SH')
GO TOP
=lfOGShowGet('M_CITYPE2',EOF('BOM'))
=lfOGShowGet('M_CITYPE3',EOF('BOM'))
=lfOGShowGet('M_CITYPE4',EOF('BOM'))
=lfOGShowGet('M_CITYPE5',EOF('BOM'))
*N119681,2 WSH, [Start] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
=lfOGShowGet('M_CITYPE6',EOF('BOM'))
=lfOGShowGet('M_CITYPE7',EOF('BOM'))

*FOR counter = 1 TO 5
FOR counter = 1 TO 7
*N119681,2 WSH, [End] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
  z = str(counter,1)
  *B603683,1 AME [Start]
  *=lfOGShowGet('M_CICOST'+Z,EOF('BOM'))
  *=lfOGShowGet('M_CISLBL'+Z,EOF('BOM'))
  *=lfOGShowGet('M_CMCOST'+Z,EOF('BOM'))
  *=lfOGShowGet('M_CMSLBL'+Z,EOF('BOM'))
  *B603683,1  AME [End]
  =lfOGShowGet('M_CMTYPE'+Z,EOF('BOM'))
ENDFOR

IF llOpBom
  =gfCloseFile('BOM')
ENDIF
*N119681,1 SSH Enabled/Disable style configuration  popup
=lfOGShowGet('M_STYCNFG',M_DYELOT="Y")
M_STYCNFG = IIF(M_DYELOT="N","N",M_STYCNFG)
*N119681,1 SSH Enabled/Disable style configuration  popup

*N119681,1 SSH Enabled/Disable style configuration  popup
=lfOGShowGet('M_CONFDEF',M_STYCNFG="Y")
M_CONFDEF = IIF(M_DYELOT="N","N",M_CONFDEF)
*N119681,1 SSH Enabled/Disable style configuration  popup

*N119681,2 WSH, [Start] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
=lfOGShowGet('M_CONFCODE',M_STYCNFG="Y")
*N119681,2 WSH, [End] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup

*!**************************************************************************
*! Name      : lfvChgDye
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of dyelot setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(),lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvChgDye()
*!**************************************************************************
FUNCTION lfvChgDye
*B601911,1 If changing from Dyelots Yes to No
IF M_DYELOT = 'N'
  lnAlias  = SELECT(0)
  lcTSTYLE = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'STYLE.DBF' AGAIN ALIAS &lcTSTYLE 
  USE (gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'STYLE.DBF') AGAIN ALIAS &lcTSTYLE 
  *E301098,1 Hesham (End)
  *B601911,1 Do not allow changing from Yes to No if there are any styles
  *B601911,1 using dyelots
  LOCATE FOR cDYE_Flg = 'Y' 
  IF FOUND()
    *B601911,1 AMM the message is  "Some Styles are in dyelots,You need to"+ 
    *B601911,1 AMM "change these Styles to dyelot no before you can change the system setup." 
    =gfModalGen("QRM00291B00000","DIALOG",'Styles|Styles')         
    M_DYELOT = 'Y'
    =lfOGShowGet('M_DYELOT',.T.)
  ENDIF
  USE IN (lcTSTYLE)
  SELECT (lnAlias)
ENDIF
*N119681,1 SSH Enabled/Disable style configuration  popup
=lfOGShowGet('M_STYCNFG',M_DYELOT="Y")
M_STYCNFG = IIF(M_DYELOT="N","N",M_STYCNFG)
*N119681,1 SSH Enabled/Disable style configuration  popup

*!**************************************************************************
*! Name      : lfvWareLoc
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of WareHouse locations setup from yes to No.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(), gfTempName(),lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvWareLoc()
*!**************************************************************************
FUNCTION lfvWareLoc
PRIVATE lnAlias,lcTWHSLoc 

*B601911,1 AMM If changed from Kepp track of locations YES to NO
IF M_WARELOC = "N"
  lnAlias = SELECT(0)
  lcTWHSLoc = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'WHSLOC.DBF' AGAIN ALIAS &lcTWHSLOC 
  USE (gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'WHSLOC.DBF') AGAIN ALIAS &lcTWHSLOC   
  *E301098,1 Hesham (End)
  GO TOP
  IF !EOF()
    *B601911,1 AMM The message is "Please note that all locations will be lost. Are you sure you want to proceed ?"
    IF gfModalGen("QRM42063B00006","DIALOG",'bins') = 2
      M_WARELOC = 'Y'
      =lfOGShowGet('M_WARELOC',.T.)
    ELSE
      *B601911,1 AMM Delete the whole warehouse locations file.
      SCATTER MEMVAR MEMO BLANK
      SCAN 
        GATHER MEMVAR MEMO
      ENDSCAN
      DELETE ALL
    ENDIF
  ENDIF  
  USE IN (lcTWHSLOC)
  SELECT (lnAlias)
ENDIF

*!**************************************************************************
*! Name      : lfvUCCDiv
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing 'Maintain U.C.C. Manf. ID ' setting.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(),lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvUCCDiv()
*!**************************************************************************
FUNCTION lfvUCCDiv

PRIVATE lnAlias,lcTCodes
*B601911,1 AMM If the setup changed from YES to NO
IF M_UCCDIV = "N"
  lnAlias = SELECT(0)
  lcTCodes = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'CODES.DBF' AGAIN ALIAS &lcTCodes
  USE (gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'CODES.DBF') AGAIN ALIAS &lcTCodes
  *E301098,1 Hesham (End)
  SET ORDER TO TAG IdRltFName
  *B601911,1 AMM Search if there is any devision exist.
  =SEEK (SyCCOMP.cComp_ID+'Y'+'CDIVISION')
  IF !EOF() 
    *B601911,1 AMM search if any devision has a related field 'CUPCMAN' not empty.
    LOCATE REST WHILE ccomp_id+crltfield+cfld_name=SyCCOMP.cComp_ID+'Y'+'CDIVISION';
                FOR cRltd_Nam='CUPCMAN' .AND. !EMPTY(cRltd_Vlu)
               
    *B601911,1 AMM the message is "UPC numbers at the division level already exist , Are you sure you want to change ?"
    IF FOUND() .AND. gfModalGen("QRM00290B00006","DIALOG",'UPC numbers at the division level')=2
      M_UCCDIV = "Y"
      =lfOGShowGet('M_UCCDIV',.T.)
    ENDIF
  ENDIF
  USE IN (lcTCodes)
  SELECT (lnAlias)
ENDIF

*!**************************************************************************
*! Name      : lfvPack
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Validate the changing of ''USE STYLE PACKS/SKU' setting.
*! REF       : *B601911,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfModalGen(),gfTempName(), lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvPack()
*!**************************************************************************
FUNCTION lfvPack
PRIVATE lcTSPCKH,lnAlias
*B601911,1 AMM If the setup changed from YES to NO.
IF M_PACK = 'N'
  lnAlias = SELECT(0)
  lcTSPCKH = gfTempName()
  SELECT 0
  *E301098,1 Hesham (Start)
  *USE ALLTRIM(SyCCOMP.cCom_DDir)+'SPCK_HDR.DBF' AGAIN ALIAS &lcTSPCKH
  USE (gfGetDataDir(ALLTRIM(SyCCOMP.cCom_DDir))+'SPCK_HDR.DBF') AGAIN ALIAS &lcTSPCKH
  *E301098,1 Hesham (End)
  GO TOP
  *B601911,1 AMM The message is "Pack and/or Sku already exist , Are you sure you want to modify the setup ?"
  IF !EOF() AND gfModalGen("QRM00290B00006","DIALOG",'Pack and\or Sku')=2
    M_PACK = 'Y'
    =lfOGShowGet('M_PACK',.T.)
  ENDIF 
  USE IN (lcTSPCKH)
  SELECT (lnAlias)
ENDIF


*:*************************************************************
*! Name     : lfvDrShpWar
*! Developer: Timour A. K.
*! Date     : 10/10/97               
*! Purpose  : Vadidate the drop ship warehouse in P/O setup.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfOpenFile() , gfBrowWare() 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDrShpWar()
*!*************************************************************
FUNCTION lfvDrShpWar

IF !EMPTY(M_DROPWARE)
  lnAlias=SELECT()
  = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  IF !SEEK(M_DROPWARE,'WAREHOUS')
    M_DROPWARE = gfBrowWare( .T. )
  ENDIF
  SELECT(lnAlias)
  SHOW GET M_DROPWARE
ENDIF


*!**************************************************************************
*! Name      : lfvClrAstm
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 11/29/1998
*! Purpose   : Validate the changing of "Use color assortments" setting.
*! REF       : *E301073,1 
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvClrAstm()
*!**************************************************************************
FUNCTION lfvClrAstm
IF MCLRASSORT = 'Y'
  *E301073,1 AMM Enable the Color assortment code POPUP
  =lfOGShowGet('MCLRASSCOD',.T.)
ELSE
  *E301073,1 AMM Initialize and disable Color assortment code POPUP
  MCLRASSCOD = SPACE(6)
  =lfOGShowGet('MCLRASSCOD',.F.)
ENDIF

*!**************************************************************************
*! Name      : lfvclrasc
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 11/29/1998
*! Purpose   : Validate the "color assortments code" setting.
*! REF       : *E301073,1 
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfOGShowGet()
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvclrasc()
*!**************************************************************************

FUNCTION lfvclrasc
PRIVATE lcBrFields, lcFile_ttl
lcCodTem = gfTempName()
SELECT 0
USE ALLTRIM(SyCCOMP.cCom_DDir)+'CODES.DBF' AGAIN ALIAS &lcCodTem
SET ORDER TO Codes
IF !EMPTY(MCLRASSCOD)
  *E301073,1 AMM If the entered color code doesn't exist
  *B602725,1 AMM Adjust to fit the new index
  *IF !SEEK(ALLTRIM(SyCCOMP.cComp_ID)+MCLRASSCOD+"N"+"COLOR") 
  IF !SEEK('N'+MCLRASSCOD+"N"+"COLOR") 
  *B602725,1 AMM end
    SET ORDER TO TAG Idrltfname
    *E301073,1 AMM If there is at least one color code in the file
    *B602725,1 AMM Adjust to fit the new index
    *IF SEEK(ALLTRIM(SyCCOMP.cComp_ID)+'N'+'COLOR')
    IF SEEK('N'+'N'+'COLOR')
    *B602725,1 AMM end
      *E301073,1 AMM browse color codes
      *E301073,1 AMM browse fields
      lcBrFields = "CCODE_NO :H='Code',CDISCREP :H='Description'"
      DIMENSION laTemp[1]
      STORE '' TO laTemp
      *E301073,1 AMM vbrowse title
      lcFile_ttl    = "Select Color"
      *E301073,1 AMM key expression
      *B602725,1 AMM Adjust to fit the new structure
      *lcFilt = ALLTRIM(SyCCOMP.cComp_ID)+'N'+'COLOR'
      lcFilt = 'N'+'N'+'COLOR'
      *B602725,1 AMM end
      =gfBrows("lcFilt",'ccode_no',"laTemp")
      MCLRASSCOD = laTemp[1]
      =lfOGShowGet('MCLRASSCOD',.T.)
    ELSE
      *E301073,1 AMM            Message
      *E301073,1 AMM     Color xxx doesn't exist
      *E301073,1 AMM        <Add>  <Reenter>
      IF gfModalGen("QRM42155B42006","DIALOG",MCLRASSCOD) = 1
        lcfldOrd = ORDER('SYDFIELD')
        SET ORDER TO TAG cfld_name IN SYDFIELD
        IF SEEK('COLOR', "SYDFIELD")
          lcRltField = UPPER(ALLTRIM(SYDFIELD.mRltFields))
          lcRltField = "|" + lcRltField + "|" 
          *E301073,1 AMM  Insert the main code record
          *B602725,1 AMM Adjust to fit the new structure of the codes file
          *INSERT INTO (lcCodTem) ;
             (ccomp_id,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cadd_user,dadd_date,cadd_time) ;
          VALUES (ALLTRIM(SyCCOMP.cComp_ID),'COLOR',MCLRASSCOD, '',"N", ;
                    gcUser_ID,DATE(),gfGetTime())
          *=gfTraceKey('CODES',ALLTRIM(SyCCOMP.cComp_ID)+PADR('COLOR',6)+MCLRASSCOD+;
                       SPACE(30)+SPACE(10),'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)
          *IF !SEEK(SPACE(2)+'N'+'COLOR') 
            *E301073,1 AMM  Insert the default code record
            *INSERT INTO (lcCodTem) ;
             (ccomp_id,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cadd_user,dadd_date,cadd_time) ;
            VALUES (SPACE(2),'COLOR',MCLRASSCOD, ;
                '',"N", gcUser_ID,DATE(),gfGetTime())
            *=gfTraceKey('CODES',SPACE(2)+PADR('COLOR',6)+MCLRASSCOD,'A',;
             ALLTRIM(SyCCOMP.cCom_DDir),SYCCOMP.mModlSet)
          *ENDIF

          INSERT INTO (lcCodTem) ;
             (cDefCode, cfld_name,ccode_no, cdiscrep,crltfield, ;
                    cadd_user,dadd_date,cadd_time) ;
          VALUES ('N' , 'COLOR', MCLRASSCOD, '' , "N" , ;
                   gcUser_ID , DATE() , gfGetTime())
          =gfTraceKey('CODES','N'+PADR('COLOR',6)+MCLRASSCOD+;
                       SPACE(30)+SPACE(10),'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)
          IF !SEEK('D'+'N'+'COLOR') 
            *E301073,1 AMM  Insert the default code record
            INSERT INTO (lcCodTem) ;
             (cDefCode,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cadd_user,dadd_date,cadd_time) ;
            VALUES ('D','COLOR',MCLRASSCOD, ;
                '',"N", gcUser_ID,DATE(),gfGetTime())
            =gfTraceKey('CODES','D'+PADR('COLOR',6)+MCLRASSCOD,'A',;
             ALLTRIM(SyCCOMP.cCom_DDir),SYCCOMP.mModlSet)
          ENDIF
          *B602725,1 AMM end
          
          *E301073,1 AMM  Get related fields
          SELECT DISTINCT sydField.cFld_name,sydField.cData_typ;
           FROM  sydField ;
           WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
             .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
           INTO  ARRAY laRelFld
          *E301073,1 AMM  Add related fields records to the code file
          FOR lnCount = 1 TO ALEN(laRelFld,1)
            *B602725,1 AMM Adjust to fit the new structure of the codes file
            *INSERT INTO (lcCodTem) ;
                   (ccomp_id,cfld_name,ccode_no, ;
                    cdiscrep,crltfield, ;
                    cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                    cadd_user,dadd_date,cadd_time) ;
            VALUES (ALLTRIM(SyCCOMP.cComp_ID),'COLOR',MCLRASSCOD, ;
                    "","Y",laRelFld[lnCount,1],;
                    laRelFld[lnCount,2],"", ;
                    gcUser_ID,DATE(),gfGetTime())
            *=gfTraceKey('CODES',ALLTRIM(SyCCOMP.cComp_ID)+PADR('COLOR',6)+;
                         MCLRASSCOD+ SPACE(30)+laRelFld[lnCount,1],'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)

            INSERT INTO (lcCodTem) ;
                   (cDefCode,cfld_name,ccode_no, ;
                    cdiscrep,crltfield, ;
                    cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                    cadd_user,dadd_date,cadd_time) ;
            VALUES ('N','COLOR',MCLRASSCOD, ;
                    "","Y",laRelFld[lnCount,1],;
                    laRelFld[lnCount,2],"", ;
                    gcUser_ID,DATE(),gfGetTime())
            =gfTraceKey('CODES','N'+PADR('COLOR',6)+;
                         MCLRASSCOD+ SPACE(30)+laRelFld[lnCount,1],'A',ALLTRIM(SyCCOMP.cCom_DDir),;
                       SYCCOMP.mModlSet)
            *B602725,1 AMM end
          ENDFOR
        ENDIF
        SET ORDER TO TAG &lcfldOrd IN SYDFIELD
      ELSE
        *E301073,1 AMM  If the user choose to reenter the color code
        _CUROBJ = _CUROBJ
      ENDIF

    ENDIF
  ENDIF
ENDIF

USE IN (lcCodTem)


*!*************************************************************
*! Name      : lfVldSets    
*! Developer : Ahmed Ibrahim
*! Date      : 11/29/1998
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid, before
*!             closing
*!*************************************************************
*! Calls     : 
*!             Procedures  : None
*!             Functions   : gfModalGen(),lfOGShowGet()
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
PRIVATE llVldSets
llVldSets = .T.

*E301073,1 AMM  If Use color assortments = 'Y' .and. color assortments code is empty
IF MCLRASSORT = 'Y' .AND. EMPTY(MCLRASSCOD)
  *E301073,1 AMM  Message 
  *E301073,1 AMM  You selected to use color assortments, but you didn't specify the 
  *E301073,1 AMM  color assortments code. Select "Reenter" to go back and edit the 
  *E301073,1 AMM  color assortments code or "Reset to Default" to change the Use 
  *E301073,1 AMM  color assortments setup. 
  *E301073,1 AMM     <Reset to Default>  <Reenter>
  
  IF gfModalGen("QRM42156B42013","DIALOG") = 2
    llVldSets = .F.
  ELSE
    MCLRASSORT = 'N'
  ENDIF
ENDIF
RETURN llVldSets


*!*************************************************************
*! Name      : lfvUseExt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 04/25/1999
*! Purpose   : Enable/Disable Extended size scale width object 
*!*************************************************************
*! Calls     : 
*!             Procedures  : None
*!             Functions   : lfOGShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   :  =lfvUseExt()
*!*************************************************************
*
FUNCTION lfvUseExt
*E301209,1 Enable/Disable Extended size scale width object [Begin]
M_EXTWIDTH = 1
=lfOGShowGet('M_EXTWIDTH',M_USEEXSSC)
*E301209,1 Enable/Disable Extended size scale width object [End  ]
*-- end of lfvUseExt.

*!*************************************************************
*! Name      : lfvChngConf
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 10/01/2003
*! Purpose   : Enabled/Disable style configuration  popup
*! REF       : N119681,1 
*!*************************************************************
*! Calls     : 
*!             Procedures  : None
*!             Functions   : lfOGShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   :  =lfvChngConf()
*!*************************************************************
*

FUNCTION lfvChngConf


*N119681,1 SSH Enabled/Disable style configuration  popup
=lfOGShowGet('M_CONFDEF',M_STYCNFG="Y")
M_CONFDEF = IIF(M_DYELOT="N","N",M_CONFDEF)
*N119681,1 SSH Enabled/Disable style configuration  popup

*N119681,2 WSH, [Start] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
FUNCTION lfValConfig
PRIVATE llValid
llValid = (M_STYCNFG="N") OR !Empty(M_CONFCODE)
RETURN llValid
*N119681,2 WSH, [End] 03/22/2004 Add Aefault configuration Code & Cost Elements 6,7 to IC Setup
