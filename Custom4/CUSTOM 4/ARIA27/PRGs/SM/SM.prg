*B600797,1 M.H 01/09/96 Variable 'LLMULCURR' not found.
*B601012,1 RENEE 04/16/96 1. Multi currency popup in the Company Setup
*B601012,1                   screen does not control the exchange rates 
*B601012,1                   differences account.
*B601012,1                2. Add function lfVldSets(), called from 
*B601012,1                   SMCMSET.PRG. It checks for the validitiy of the
*B601012,1                   entered values in the Comany Setups grid before
*B601012,1                   closing.
*B601298,1 HSS_Haytham El_Sheltawi Corecting the fiscal year preods whin 
*B601298,1                         the the fiscal year is 1996
*E300683,6 RENEE 06/04/97 - Add BMP variables
*E300687,1 AHMED 06/16/97 - validate some entry on closing the setups grid
*E300693,1 Hesham El_Sheltawi 06/22/97
*E300693,1 add arrays and variables needed for printing the default printer
*E300693,1 for different screens
*E300692,5 RENEE 07/02/97 Move lfAddFsYer function to SMFSYER.PRG program
*E300459,1 Reham On 08/07/97
*E300459,1 Add new function lfvAdjAcct()to validate cAdjAcct 
*E300459,1 {Related field for Debit & Credit Adj reason codes}
*E300459,1 This function called from the related fields screen
*E300459,1 {SMRELFD} that branched from codes screen. {SMCODES}
*E300459,1 Reham On 08/10/97
*E300459,1 Add new function to the get the picture & the valid function
*E300459,1 for the related field {cAdjAScct} for both debit & credit 
*E300459,1 adjustement reason codes in the codes screen & the related 
*E300459,1 field screen.
*B601911,1 AMM  02/12/98 Bugs in the GL settings and add anew function lfvTaxSt()
*B601911,1 AMM           to validate the tax setting
*E300788,4 AMM  03/12/98 Call the GL link screen to enter the default link 
*E300788,4 AMM           code if change LINK GL from NO to YES and the default 
*E300788,4 AMM           link code doesn't exist.
*E300930,1 AMM  11/04/98 Add some related fields validations (Link_code, 
*E300930,1 AMM            Sales link code, division group)
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B602556,1 RENEE02/17/99 Fix 'File is in use' bug in lfvAdjAcct()
*B602626,1 AMM  03/01/99 Fix the bug of 'File is not exist' in lfvMFGOpr()
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*B602658,1  HS  03/10/99 Fix some bugs that has appeared because of the
*B602658,1                remove of the field cComp_ID from the files
*B602658,1                FISHD , FSPRD.
*E301173,1 AMM  03/17/99 Don't save the gl_company when select to link to GL Aria type.
*E301176,1 HDM  03/21/1999 Define new Variables as The Notepad Controled Globally
*B602589,1 AMM  03/22/1999 Fix some bugs in lfvLnksls(),lfvlnkCod(),lfvDivGrp()
*E301210,1 ASH  04/27/1999 Add M_GL_COST validation in lfOGRShow().
*E301217,1 AMM  05/10/99 Add new related field "EOM day " to the payment terms code
*B602889,1 AMM  11/05/99 Fix the bug of "SYSDATA" does not exist
*E301268,1 HDM  07/01/99 Filter vendor browse according to Supp. Type = 'C' --> Contractor
*E301297,1  HS  07/22/99   Implement the Object/Events triggers, Add a new
*E301297,1                 function (gfDoTriger) to handle the Triggers that
*E301297,1                 need to be executed when a certain event takes
*E301297,1                 place from a certain screen (object).
*B603098,1 AMM  08/07/1999 Add validations to Sequence # based on div. 
*B603098,1 				    setting in company setups
*E301315,1 Hesham 09/13/99
*E301315,1 If the user is running the setup program for a module that does not
*E301315,1 have an application then run the SM.app insted of the module .app
*B802700,1  WA  10/24/99 Force entering value to Operation Sequence 
*B802700,1               if Consider as operation is "YES".
*B802792,1 WAB  01/05/2000 display the option 'Link To GL' enabled or disable 
*B802792,1 WAB             depend if there are any modules can linked to gl
*B802792,1 Reham04/24/2000 Fix the bugs generated from B802792.
*B603468,1 NAD  02/22/2000 Don't allow modification in UCC Manufacturer id when UPCs already
*B603468,1 NAD               Generated using this manufacturer id
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field
*B603744,1 ADEL 07/19/2000 Fix the bug of 'StyleUpc' file doesn't exist in case of no active company selected.
*B603925,1 HBG  08/10/2000 Fix the bug of 'Sequence' file doesn't exist in case of no active company selected.
*E301469,1 AMH  10/22/2000 Purge Database.
*E301488,1 MAB  12/03/2000 Site PreFix is a company setup.
*E301488,1 MAB  12/03/2000 Validation for site type (No more than one Back Office).
*B604095,1 AMH  01/08/2001 Fix Hanging in Company Setups Screen.
*C102212,1 ADEL 03/14/20001 Add HST tax to setup and Enable it according to "Use Tax".
*B604337,1 AMM  03/29/2001 Need to have the ability to leave the Unique site prefix empty
*B804136,1 SSH             Fix APSETUP not found.[Start]
*B604470,1 NAD  05/22/2001 Even if the EOM flag is "No"  you are still be forced to put in a number between 1 and 31	
*B605279,1 WAB  03/11/2002 fix the bug of the UCC code in the company setup is always disable  
*B804062,1 NAD  04/09/2002 Err C:\sequence.dbf not found
*B607348,1 ABD 08/04/2003 Don't print all UK on A4 papper, add seting to print on A4 Paper.
*E038226,1 SMM 08/01/2004 Validate Logo Path	
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
************************************************************************************
*:Ver 2.0 {12/25/94}
PARAMETERS lcProgName,lcprgpram
lcprgpram = IIF(TYPE('lcprgpram')='C',lcprgpram,'')
#REGION 1
*E300693,1 Hesham El_Sheltawi (Start)
*DECLARE   laData[1,1],laDataCopy[1,1],laFld_msg[1,2],laScrMode[5,1],laKeyField[3,1],;
          laDefProc[10,1],laFileName[1,2],laField_H[1,1],laField_N[1,1],;
          laWndObj[1,3],laFiltrExp[1,7],laFixFltr[1,7],laCtrStat[12],laWobjects[1,3],laArrayes[1,2],laSubProc[1,2],;
          laOGFxFlt[1,7],laOGVrFlt[1,7]
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 
*DECLARE   laData[1,1],laDataCopy[1,1],laFld_msg[1,2],laScrMode[5,1],laKeyField[3,1],;
          laDefProc[10,1],laFileName[1,2],laField_H[1,1],laField_N[1,1],;
          laWndObj[1,3],laFiltrExp[1,7],laFixFltr[1,7],laCtrStat[12],laWobjects[1,3],laArrayes[1,2],laSubProc[1,2],;
          laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]


*-HDM E301176,1 03/21/1999[Start] Changed laCtrStat[12] To laCtrStat[13]

*E301297,1 Change this line to add a new row to the array (laCtrStat) for
*          the Audit Trail button [Begin]
*DECLARE   laData[1,1],laDataCopy[1,1],laFld_msg[1,2],laScrMode[5,1],laKeyField[3,1],;
*          laDefProc[10,1],laFileName[1,2],laField_H[1,1],laField_N[1,1],;
*          laWndObj[1,3],laFiltrExp[1,7],laFixFltr[1,7],laCtrStat[13],laWobjects[1,3],laArrayes[1,2],laSubProc[1,2],;
*          laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1]
DECLARE   laData[1,1],laDataCopy[1,1],laFld_msg[1,2],laScrMode[5,1],laKeyField[3,1],;
          laDefProc[10,1],laFileName[1,2],laField_H[1,1],laField_N[1,1],;
          laWndObj[1,3],laFiltrExp[1,7],laFixFltr[1,7],laCtrStat[15],laWobjects[1,3],laArrayes[1,2],laSubProc[1,2],;
          laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1]

*E301297,1 Change this line to add a new row to the array (laCtrStat) [End]

*-HDM E301176,1 03/21/1999[End]

*E301164,1 Hesham (End)
*E300693,1 end
STORE .F. TO llAddRec,llEditRec,llDeleRec           
STORE  1  TO lnRecNo,lnCurObj,lnBar_No,lnDataNo,pbRight,pbLeft,pbButt

*E300693,1 Hesham El_Sheltawi (Start)
*STORE '' TO laKeyField,laFileName,laWndObj,laField_N,laField_H,laPrgNames,laArrayes,;
            lcWindTitl,lcFileName,lcScFields,lcFile_Ttl,lcBrFields,lcBaseFile,lcWinAppl,;
            lc_TablNam,lcStamp,lcPop_Name,laFiltrExp,laFixFltr,;
            lcFileFltr,laWobjects,lcButtNam,laSubProc,lcReportID,laOGFxFlt,laOGVrFlt

*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 
*STORE '' TO laKeyField,laFileName,laWndObj,laField_N,laField_H,laPrgNames,laArrayes,;
            lcWindTitl,lcFileName,lcScFields,lcFile_Ttl,lcBrFields,lcBaseFile,lcWinAppl,;
            lc_TablNam,lcStamp,lcPop_Name,laFiltrExp,laFixFltr,;
            lcFileFltr,laWobjects,lcButtNam,laSubProc,lcReportID,laOGFxFlt,laOGVrFlt,;
            laRepFltr
*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally
*STORE '' TO laKeyField,laFileName,laWndObj,laField_N,laField_H,laPrgNames,laArrayes,;
            lcWindTitl,lcFileName,lcScFields,lcFile_Ttl,lcBrFields,lcBaseFile,lcWinAppl,;
            lc_TablNam,lcStamp,lcPop_Name,laFiltrExp,laFixFltr,;
            lcFileFltr,laWobjects,lcButtNam,laSubProc,lcReportID,laOGFxFlt,laOGVrFlt,;
            laRepFltr,laPrgTemps,lcMenUnProce

STORE '' TO laKeyField,laFileName,laWndObj,laField_N,laField_H,laPrgNames,laArrayes,;
            lcWindTitl,lcFileName,lcScFields,lcFile_Ttl,lcBrFields,lcBaseFile,lcWinAppl,;
            lc_TablNam,lcStamp,lcPop_Name,laFiltrExp,laFixFltr,;
            lcFileFltr,laWobjects,lcButtNam,laSubProc,lcReportID,laOGFxFlt,laOGVrFlt,;
            laRepFltr,laPrgTemps,lcMenUnProce,;
            lcSydKey     , lcNoteType      , lcNoteKey
*-- HDM E301176,1 03/21/1999 [End]

*E301164,1 Hesham (End)
*E300693,1 end

laCtrStat[1]  = "DISABLE"                && First button    
laCtrStat[2]  = "DISABLE"                && Last button
laCtrStat[3]  = "DISABLE"                && Next button
laCtrStat[4]  = "DISABLE"                && Priviious button   
laCtrStat[5]  = "DISABLE"                && Copy button
laCtrStat[6]  = "DISABLE"                && Past Button
laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "ENABLE"                 && Browse button
laCtrStat[11] = "DISABLE"                && Save button 
laCtrStat[12] = "ENABLE"                 && Close/Cancel button 

laCtrStat[13] = "DISABLE"
  
*E301297,1 Add this line to add a new row to the array (laCtrStat) for the
*          Audit Trail button [Begin]
laCtrStat[14]  = "DISABLE"                && User Fields button
laCtrStat[15]  = "DISABLE"                && Audit Trail button
*E301297,1 Add this line to add a new row to the array (laCtrStat) [End]

laScrMode    = .F.
laScrMode[1] = .T.
laFld_msg    = SPACE (51)
laDefProc    = .T.

lcSelCont  = SCHEME(1,6) 
lcEnbCont  = SCHEME(1,2) 
lcDisCont  = SCHEME(1,10) 
lcBaseWind = " "
lcLoclShow = "lpShow"
llUserSet  = .F.
llMultiRun = glMultiRun
llUpdate   = .F.
llCUpdate  = .F.
llglobShow = .T.
llNoContrl = .F. 
llFromBrow = .F.
llDoLocal  = .F.
glQuitting = .F.   

lnDateWdth = IIF('ON'$SET('CENT'),10,8)

*E300683,6 Add variables holding BMP names for all BMPS use in all AP screens
lcBtMpUp1  = gcBMPHome + "UP1.BMP"
lcBtMpDn1  = gcBMPHome + "DN1.BMP"
lcBtMpPrc  = gcBMPHome + "PROCEED.BMP"
lcBtMpNew  = gcBMPHome + "NEW.BMP"
lcBtMpRem  = gcBMPHome + "REM.BMP"
lcBtMpCan  = gcBMPHome + "CAN.BMP"
lcBtMpCls  = gcBMPHome + "CLS.BMP"
lcBtMpOK   = gcBMPHome + "OK.BMP"
lcBtMpExt  = gcBMPHome + "EXTKEY.BMP"
lcBtMpEdt2 =  gcBMPHome + "EDIT2.BMP"
lcBtMpZoom =  gcBMPHome + "ZOOM.BMP"
lcBtMpSav  =  gcBMPHome + "SAV.BMP"
lcBtMpSave =  gcBMPHome + "SAVE.BMP"
lcBtMpSel  =  gcBMPHome + "SEL.BMP"
lcBtMpIns  =  gcBMPHome + "INSERT.BMP"
lcBtMpRec  =  gcBMPHome + "RECALL.BMP"
*E300683,6 end

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


ON KEY LABEL CTRL+W         lnDummy = 1
ON KEY LABEL Ctrl+Q         lnDummy = 1
ON KEY LABEL CTRL+END       lnDummy = 1
ON KEY LABEL CTRL+HOME      lnDummy = 1

*E301434,1 Ramy Add this line to trap F3 key [start]
lcF3Key = ON('KEY' , 'F3')
ON KEY LABEL F3               DO lfF3Key
*E301434,1 Ramy [end]


lcPrgTmp = IIF(ATC(' ',lcProgName)>0,SUBSTR(lcProgName,1,ATC(' ',lcProgName)-1),lcProgName)
*gcDosApps = gcAppHome+gcWinAppl+'\'
gcDosApps = gcAppHome+'SM\'

*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
IF GCMULTIINST 
  gcCDosApps = gcCappHome+'SM\'
ENDIF   
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]

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
ENDIF  

*E301434,1 Ramy [start]
*ON KEY LABEL ESC 
ON KEY LABEL ESC &lcESC
ON KEY LABEL F3 &lcF3Key
*E301434,1 Ramy [end]

#REGION 1

=gfCleanUp()




*!**************************************************************************
*!
*!      Function lfvDfnBy
*!
*!**************************************************************************
*
FUNCTION lfvDfnBy
*
PARAMETER lnAryElm

laData[lnAryElm] = SUBSTR('SAU',rbDfnBy)

*!**************************************************************************
*!
*!      Function lfwSetClor
*!
*!**************************************************************************
*
Function lfwSetColor

PARAMETER lcOnjName
lcOldColor = &lcOnjName

*!**************************************************************************
*!
*!      Function lfvSetColor
*!
*!**************************************************************************
*
FUNCTION lfvSetColor

PARAMETER lcOnjName

IF !EMPTY((lcOnjName)) .AND. LASTKEY() = 13
  DECLARE laColorSet[1,1]
  lsColorSet = 1
  
  SELECT NAME FROM (gcSysHome+"SYCRESRC") ;
         WHERE ID ='COLORSET'       ;
         INTO ARRAY laColorSet
  
  IF USED ('SYCRESRC')
    USE IN SYCRESRC
  ENDIF
         
  IF ASCAN(laColorSet,ALLTRIM(&lcOnjName)) = 0 
    DO  SMCLSET.SPR       
    IF  lsColorSet = 0
      &lcOnjName = lcOldColor 
    ELSE  
      &lcOnjName = laColorSet[lsColorSet]
    ENDIF
  ENDIF

  SHOW GET (lcOnjName)
ENDIF  

*!**************************************************************************
*!
*!      Function :lfwVldWrk
*!
*!**************************************************************************
*
FUNCTION lfwVldWrk
PARAMETERS lcObj_Nam

*** Save old user Id so if you cancel from brow restor it back
lcOld_ID = &lcObj_Nam

*!**************************************************************************
*!
*!      Function: lfvVldWrk
*!
*!**************************************************************************
*
* Function to validate the user id and display the brow from user file
* if you enter unlisted user id to select from
*
FUNCTION lfvVldWrk
PARAMETERS lcObj_Nam
DECLARE laWrkInfo[2,1]

IF TYPE('lcWrk_Name')='U'
  lcWrk_Name = ''
ENDIF  

*** You are not permited to leave this filed empty
IF EMPTY(&lcObj_Nam) AND LASTKEY()= 13 
  RETURN .F.
ENDIF

IF !EMPTY(&lcObj_Nam)

  lcCurFile      = ALIAS()
  SELECT SYCWRKST
  *** If found update the user name from user file

  IF SEEK(&lcObj_Nam,"SYCWRKST")
    lcWrk_Name = sycWrkst.cWrk_name
  ELSE
    
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow     =  lcBrFields  && Save old valu of the variable
    lcBrFields     =  "cwrkst_id :H='WorkStation ID',cwrk_Name :H='WorkStation Name',"+;
                      "cwrk_loc :H='WorkStation Location',cwrk_sNum :H='WorkStation Number'"
    lcSav_ttl      = lcFile_Ttl
    lcFile_Ttl     = "WorkStation Information"
  
    laWrkInfo [1] = (lcObj_Nam)
    laWrkInfo [2] = lcWrk_Name



    =gfBrows(.F.,"cwrkst_id " ,"laWrkInfo")
    
    lcBrFields = lcSaveBrow 
    lcFile_Ttl = lcSav_ttl


    *** If cancel from the brow you will come back  with the same id
    IF (lcObj_Nam) = laWrkInfo[1] 
      &lcObj_Nam  = lcOld_ID
    ELSE
      &lcObj_Nam  = laWrkInfo[1]
      lcWrk_Name = laWrkInfo[2]
    ENDIF
  ENDIF  
  SELECT (lcCurFile)
ENDIF

SHOW GET (lcObj_Nam)

*!**************************************************************************
*!
*!      Function: lfGetIndExp
*!
*!**************************************************************************
*
FUNCTION lfGetIndExp

lcExp = SYS(14,VAL(SYS(21)))
RETURN lcExp

*!**************************************************************************
*!
*!      Function: lfGetIndTag
*!
*!**************************************************************************
*
FUNCTION lfGetIndTag

lcTag = ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
RETURN lcTag

*!********************************************************************
*!
*!              Function: lfAddPrds
*!
*!********************************************************************
*
FUNCTION lfAddPrds

PARAMETERS lcComp_ID,lcFisYear,lnPeriods,lnCurPrd,ldStartDate,ldEndDate,;
           lcFileName

lnYearDays  = (ldEndDate-ldStartDate)+1
lnPrdDays   = INT(lnYearDays/lnPeriods)
lnPrdMonth  = 12/lnPeriods
ldPrdBegn   = ldStartDate 
ldPrdEnd    = IIF(MOD(12,lnPeriods)>0 .OR. lnYearDays<365,;
                  ldPrdBegn+lnPrdDays-1,GOMONTH(ldPrdBegn,lnPrdMonth)-1)
lnPart      = 0
FOR lnCount = 1 TO lnPeriods-1
  lnPart = IIF(MOD(lnCount-1,3)=0,lnPart+1,lnPart)

  *B602658,1 Change this line because we have removed the field cComp_ID
  *          from the file [Begin]
  *INSERT INTO &gcWorkDir.&lc_TempPR ;
  *       (cComp_ID,cFisFyear,cFspprdid,;
  *       cfsppdesc,dFsppbgdt,dFsppendt,;
  *       nFsppartn,lFspclsds,cStatus);
  *       VALUES(lcComp_ID,lcFisYear,RIGHT("0"+ALLTRIM(STR(lnCount)),2),;
  *              CMONTH(ldPrdBegn),ldPrdBegn,ldPrdEnd,;
  *              lnPart,IIF(lnCount<lnCurPrd,.T.,.F.),'A')
  INSERT INTO &gcWorkDir.&lc_TempPR ;
         (cFisFyear,cFspprdid,;
         cfsppdesc,dFsppbgdt,dFsppendt,;
         nFsppartn,lFspclsds,cStatus);
         VALUES(lcFisYear,RIGHT("0"+ALLTRIM(STR(lnCount)),2),;
                CMONTH(ldPrdBegn),ldPrdBegn,ldPrdEnd,;
                lnPart,IIF(lnCount<lnCurPrd,.T.,.F.),'A')
  *B602658,1 Change this line because we have removed the field cComp_ID [End]
  
  ldPrdBegn = IIF(MOD(12,lnPeriods)>0.OR. lnYearDays<365,;
                  ldPrdBegn+lnPrdDays  ,GOMONTH(ldPrdBegn,lnPrdMonth)  ) 
  ldPrdEnd  = IIF(MOD(12,lnPeriods)>0.OR. lnYearDays<365,;
                  ldPrdBegn+lnPrdDays-1,GOMONTH(ldPrdBegn,lnPrdMonth)-1)
ENDFOR

ldPrdEnd    = ldEndDate
lnPart      = IIF(MOD(lnCount-1,3)=0,lnPart+1,lnPart)

*B602658,1 Change this line because we have removed the field cComp_ID
*          from the file [Begin]
*INSERT INTO &gcWorkDir.&lc_TempPR ;
*       (cComp_ID,cFisFyear,cFspprdid,cfsppdesc,dFsppbgdt,dFsppendt,nFsppartn,lFspclsds,cStatus);
*       VALUES(lcComp_ID,lcFisYear,RIGHT("0"+ALLTRIM(STR(lnCount)),2),CMONTH(ldPrdBegn),;
*                 ldPrdBegn,ldPrdEnd,lnPart,IIF(lnCount<lnCurPrd,.T.,.F.),'A')
INSERT INTO &gcWorkDir.&lc_TempPR ;
       (cFisFyear,cFspprdid,cfsppdesc,dFsppbgdt,dFsppendt,nFsppartn,lFspclsds,cStatus);
       VALUES(lcFisYear,RIGHT("0"+ALLTRIM(STR(lnCount)),2),CMONTH(ldPrdBegn),;
                 ldPrdBegn,ldPrdEnd,lnPart,IIF(lnCount<lnCurPrd,.T.,.F.),'A')
*B602658,1 Change this line because we have removed the field cComp_ID [End]

*!**************************************************************************
*!
*!      Function: gfUpdate
*!
*!**************************************************************************
*
FUNCTION gfUpdate

*** Check if you confirm action on one of the objects and chang the llUpdate
*** flag, and confirm that llOnChang flag still .T.
IF UPDATE()
  llCUpdate  = .T.
ENDIF  

*!**************************************************************************
*!
*!      Function: gfFieldPic
*!
*!**************************************************************************
*
FUNCTION gfFieldPic
PARAMETERS lcFld_Name
PRIVATE lcFld_Name,lcPicture

lcPicture=''

lcFld_type = TYPE(lcFld_Name)

DO CASE
   CASE lcFld_type = 'C'
     lcPicture = REPLICATE("X",FSIZE(lcFld_Name))
   CASE lcFld_type = 'D'
     lcPicture = "@D"
ENDCASE

RETURN lcPicture

*!**************************************************************************
*!
*!      FUNCTION: lfAlpha
*!
*!**************************************************************************
* Author : Hesham Alsheltwy
*
FUNCTION lfAlpha

PARAMETERS lcString

*** Func. to know if the sended value have any Digits. ***
*** or the value sended is Alpha. ***

llString = .T.
lnCount  = 1

DO WHILE lnCount <= LEN(lcString) .AND. llString
  llString = ISALPHA(SUBSTR(lcString,lnCount,1))
  lnCount = lnCount + 1
ENDDO

RETURN llString

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
*!      Function: lfOGRShow
*!
*!**************************************************************************
*
FUNCTION lfOGRShow
*E301315,1 Hesham (Start)
*E301315,1 If the user is running the setup program for a module that does not
*E301315,1 have an application Return
IF LCOGMODLE <> CHR(255)+CHR(255)
  RETURN
ENDIF
*E301315,1 Hesham (End)
*B802792,1 WAB - call Function to cheack if there are any modules can linked to gl
*--------------- llGlLink = .T. ----> there are modules can linked to GL
llGlLink = lfLnkModul()
*E301469,1 AMH [Start]
llHist = GfGetMemVar('LLHIST',laData[1])
lcHisCmpID = GfGetMemVar('M_COMP_ID',laData[1])
*E301469,1 AMH [End  ]

*B802792,1 Reham On 04/24/2000  [Start]
*B802792,1 If there is no link default the bar to No else default to the default value.
M_LINK_GL = IIF(!llGlLink , "N" , M_LINK_GL)
*B802792,1 Reham On 04/24/2000  [End]
*B802792,1 WAB - END
DO CASE
  CASE laScrMode[3]
  
    llMultCCnt=IIF(LLMULCURR,.F.,.T.) 
    llMultCurr=IIF(LLMULCURR,.T.,.F.)
    *B603468,1 NAD (Start) Don't allow modification in UCC Manufacturer id when UPCs already 
    *B603468,1 NAD         Generated using this manufacturer id
    =lfOGShowGet('XMANUFID',lfChkUcc())
    *B603468,1 NAD (End)
    =lfOGShowGet('LLMULCURR',llMultCCnt)
    =lfOGShowGet('LLEXCHRATE',llMultCurr)
    =lfOGShowGet('LLSTYPRICE',llMultCurr)
    =lfOGShowGet('LLEDITEXRA',llMultCurr)
    =lfOGShowGet('LNEXRATDAY',llMultCurr)    
    *B601012,1 Show LNEXRATACC as well
    =lfOGShowGet('LNEXRATACC',llMultCurr)
    *B601012,1 end.
    *E300687,1 start
    *B802792,1 WAB - display the option 'Link To Gl' ENABLED if llGllink is true 
    *=lfOGShowGet('M_LINK_GL',.T.)
    =lfOGShowGet('M_LINK_GL',llGlLink)
    *B802792,1 WAB - END
    =lfOGShowGet('M_GL_VERS',M_LINK_GL='Y')
    =lfOGShowGet('M_SYS_DIR',M_LINK_GL='Y' .AND. M_GL_VERS='S')
    =lfOGShowGet('M_GL_CO',M_LINK_GL='Y' .AND. M_GL_VERS='S')
    =lfOGShowGet('M_POST_DET',M_LINK_GL='Y')
    =lfOGShowGet('M_DIV_LINK',M_LINK_GL='Y')
    *E300687,1 end
    *E301210,1 ASH 04/27/1999 (Begin) Disable Gl cost setting in case of not link with G/L.
    =lfOGShowGet('M_GL_COST',M_LINK_GL='Y')
    *E301210,1 ASH 04/27/1999 (End)
    *B601911,1 AMM start, Disable the taxes choices if the tax status is NO
    =lfOGShowGet('M_TAX_DESC',M_TAX = 'Y')    
    =lfOGShowGet('M_TAX_METH',M_TAX = 'Y')        
    =lfOGShowGet('M_TAX_RATE',M_TAX = 'Y')    
    =lfOGShowGet('M_TAX_REFE',M_TAX = 'Y')    
    *B601911,1 AMM end

    *E301469,1 AMH [Start]
    =lfOGShowGet('LLHIST',IIF(!llHist .AND. EMPTY(lcHisCmpID),.T.,.F.))
    =lfOGShowGet('M_COMP_ID',llHist .AND. EMPTY(lcHisCmpID))
    *E301469,1 AMH [End  ]
    
    *B607348,1 ABD - Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
    =lfOGShowGet('M_LLUSEPA4',gcContCode = "ENG")
    *B607348,1 ABD - [End]

  CASE laScrMode[4]
    llMultCCnt=.T.
    *B600797,1 M.H 01/09/96 Variable 'LLMULCURR' not found.
    *lcMultCurr=IIF(LLMULCURR,.T.,.F.)
    llMultCurr=IIF(LLMULCURR,.T.,.F.)
    *B600797,1 M.H End.
    *B603468,1 NAD (Start) Don't allow modification in UCC Manufacturer id when UPCs already 
    *B603468,1 NAD         Generated using this manufacturer id
    =lfOGShowGet('XMANUFID',lfChkUcc())
    *B603468,1 NAD (End)
    =lfOGShowGet('LLMULCURR',llMultCCnt)
    =lfOGShowGet('LLEXCHRATE',llMultCurr)
    =lfOGShowGet('LLSTYPRICE',llMultCurr)
    =lfOGShowGet('LLEDITEXRA',llMultCurr)
    =lfOGShowGet('LNEXRATDAY',llMultCurr)
    *B601012,1 Show LNEXRATACC as well
    =lfOGShowGet('LNEXRATACC',llMultCurr)
    *B601012,1 end.
    *B802792,1 WAB - display the option 'Link To Gl' ENABLED if llGllink is true 
    =lfOGShowGet('M_LINK_GL',llGlLink)
    *B802792,1 WAB - END
    *B601012,1 Disable objects in View mode

    *E301469,1 AMH [Start]
    =lfOGShowGet('LLHIST',IIF(!llHist .AND. EMPTY(lcHisCmpID),.T.,.F.))
    =lfOGShowGet('M_COMP_ID',llHist .AND. EMPTY(lcHisCmpID))
    *E301469,1 AMH [End  ]

    *B607348,1 ABD - Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
    =lfOGShowGet('M_LLUSEPA4',gcContCode = "ENG")
    *B607348,1 ABD - [End]

  CASE laScrMode[2]
    =lfOGShowGet('LLMULCURR',.F.)
    =lfOGShowGet('LLEXCHRATE',.F.)
    =lfOGShowGet('LLSTYPRICE',.F.)
    =lfOGShowGet('LLEDITEXRA',.F.)
    =lfOGShowGet('LNEXRATDAY',.F.)
    =lfOGShowGet('LNEXRATACC',.F.)
    *E300687,1 start
    =lfOGShowGet('M_LINK_GL',.F.)
    =lfOGShowGet('M_GL_VERS',.F.)
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
    =lfOGShowGet('M_POST_DET',.F.)
    =lfOGShowGet('M_DIV_LINK',.F.)
    =lfOGShowGet('M_DIV_SEQ',.F.)
    =lfOGShowGet('XDUNS',.F.)
    =lfOGShowGet('XMANUFID',.F.)
    =lfOGShowGet('XUPSFROM',.F.)            
    =lfOGShowGet('XUPSACCT',.F.)
    =lfOGShowGet('M_TAX',.F.)
    =lfOGShowGet('M_TAX_DESC',.F.)
    =lfOGShowGet('M_TAX_METH',.F.)
    =lfOGShowGet('M_TAX_RATE',.F.)
    =lfOGShowGet('M_TAX_REFE',.F.)
    *E300930,1 AMM disable the system type setting in the view mode
    =lfOGShowGet('M_SYSTYPE',.F.)
    *E301210,1 ASH 04/27/1999 (Begin) Disable the GL_COST setting in the view mode.
    =lfOGShowGet('M_GL_COST',.F.)
    *E301210,1 ASH 04/27/1999 (End)
    *E300930,1 AMM end
    *E300687,1 end

    *E301469,1 AMH [Start]
    =lfOGShowGet('LLHIST',.F.)
    =lfOGShowGet('M_COMP_ID',.F.)
    *E301469,1 AMH [End  ]
    *B604337,1 AMM Disable the setting in view mode 
    =lfOGShowGet('M_UNQSTPRX',.F.) 
    =lfOGShowGet('M_BOMVAR',.F.) 
    =lfOGShowGet('M_CONFIRM',.F.)
    *B604337,1 AMM end
    *C102212,1 (Begin) Disanable it.
    =lfOGShowGet('M_HST_RATE',.F.)
    *C102212,1 (End)

    *B607348,1 ABD - Don't print all UK on A4 papper, add seting to print on A4 Paper. [Begin]
    =lfOGShowGet('M_LLUSEPA4',.F.)
    *B607348,1 ABD - [End]

ENDCASE
*C102212,1 (Begin) Disanable it is any country rather CANADA.
IF UPPER(ALLTRIM(gcContCode))<>'CANADA'
  =lfOGShowGet('M_HST_RATE',.F.)
ENDIF  
*C102212,1 (End)


*!**************************************************************************
*!
*!      Function: lfOGRValid
*!
*!**************************************************************************
*
FUNCTION lfOGRValid
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfOGRWhen
*!
*!**************************************************************************
*
FUNCTION lfOGRWhen
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfvMulCurr
*!
*!**************************************************************************
*
FUNCTION lfvMulCurr
IF LLMULCURR
  llMultCurr = .T.
  LLEXCHRATE = .T.
  LLSTYPRICE = .T.
  LLEDITEXRA = .F.
  LNEXRATDAY = 30
ELSE
  llMultCurr = .F.
  LLEXCHRATE = .F.
  LLSTYPRICE = .F.
  LLEDITEXRA = .F.
  LNEXRATDAY = 0
ENDIF
=lfOGShowGet('LLMULCURR',.T.)
=lfOGShowGet('LLEXCHRATE',llMultCurr)
=lfOGShowGet('LLSTYPRICE',llMultCurr)
=lfOGShowGet('LLEDITEXRA',llMultCurr)
=lfOGShowGet('LNEXRATDAY',llMultCurr)
*B601012,1 Clear the exch rate diff account field and show according
*B601012,1 to the multi currency state (
LNEXRATACC = SPACE(24)
=lfOGShowGet('LNEXRATACC',llMultCurr)
*B601012,1 end.


*!*************************************************************
*! Name      : lfVldSets                   B601012,1
*! Developer : Renee Ezzat
*! Date      : 04/16/1996
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

*E301315,1 Hesham (Start)
*E301315,1 If the user is running the setup program for a module that does not
*E301315,1 have an application Return
IF LCOGMODLE <> CHR(255)+CHR(255)
  RETURN
ENDIF
*E301315,1 Hesham (End)

llVldSets = .T.
*llvldcom  = .T.
*B601911,1 AMM Move this part from below to here to exclude it from the DOCASE
*B601911,1 AMM of the GL settings.
*IF LLMULCURR 
IF LLMULCURR .AND. EMPTY(lnExRatAcc)
  =gfModalGen("TRM00250B00000","DIALOG",'exchange difference account')
  _CUROBJ   = OBJNUM(lnExRatAcc)
  llVldSets = .F.
ENDIF
*B601911,1 AMM end
DO CASE
  *B601012,1 If the account type is 
  *B601911,1 AMM comment out, Move it UP to exclude it from the DOCASE of the 
  *B601911,1 AMM GL settings.
  *CASE EMPTY(lnExRatAcc)
    *=gfModalGen("TRM04066B00000","DIALOG",'the exchange difference account')
    *_CUROBJ   = OBJNUM(lnExRatAcc)
    *llVldSets = .F.
  *B601911,1 AMM end
  *B601911,1 AMM, move this part from bottom to here, to check on it, first 
  *B601911,1 AMM  of all.
  CASE EMPTY(M_GL_VERS) .AND. M_LINK_GL='Y'
    =gfModalGen('INM00250B00000','DIALOG','G/L Version')
    llVldSets = .F.
  *B601911,1 AMM end  

  *E300687,1 start
  CASE EMPTY(M_SYS_DIR) AND M_GL_VERS='S'
    *=gfDialog('I','The SBT System Directory directory cannot be left empty')
    =gfModalGen('INM00250B00000','DIALOG','SBT System Directory')
    _CUROBJ     = OBJNUM(M_SYS_DIR)
    llVldSets   =.F.
  CASE EMPTY(M_GL_Co) AND M_GL_VERS='S'
    *=gfDialog('I', 'The SBT Company cannot be left empty')
    =gfModalGen('INM00250B00000','DIALOG','SBT Company ')
    llVldSets   = .F.
  CASE EMPTY(M_POST_DET) AND M_LINK_GL = 'Y'
    M_POST_DET = 'D'
    =lfOGShowGet('M_POST_DET',.T.)
    IF EMPTY(M_DIV_LINK) AND M_LINK_GL = 'Y'
      M_DIV_LINK  = 'N'
      =lfOGShowGet('M_DIV_LINK',.T.)
    ENDIF
  CASE EMPTY(M_DIV_LINK) AND M_LINK_GL = 'Y'
    M_DIV_LINK  = 'N'
    =lfOGShowGet('M_DIV_LINK',.T.)
  *B601911,1 AMM start, move this part up to check on it first of all
  *CASE EMPTY(M_GL_VERS) 
    *=gfDialog('I', 'G/L Version cannot be empty')
    *=gfModalGen('INM00250B00000','DIALOG','G/L Version')
    *llVldSets = .F.
  *B601911,1 AMM end  
    *E300687,1 end
ENDCASE
*E300788,4 AMM  If there is valid link to GL Check for the default link codes.
IF M_LINK_GL='Y' .AND. llVldSets = .T.
  lcTempGL = gfTempName()
  SELECT 0
  USE (ALLTRIM(laData[11])+'GL_LINK') AGAIN ALIAS &lcTempGL ORDER GL_LINK
  IF !SEEK('DEF')
    lcCurrWind = gcBaseWind
    =gfStatic()
    lcWindow   = UPPER(lcBaseWind)
    glFirsTime = .T.
    gcBaseWind ='AWRSMGLLNK'
    *E300788,4 AMM Call Link codes screen with parameters (Company,GL version)
    DO (gcAppHome+'SM') WITH 'SMGLLNK','"'+laData[1]+'"'+','+"'"+M_GL_VERS+"'"
    gcBaseWind = lcCurrWind
    glFirsTime = .F.
    *E300788,4 AMM Restore the Old environment.
    =gfsetup()
    SHOW GETS LEVEL 2
    SELECT (lcTempGL)
    *E300788,4 AMM Check if the user entered default link codes, if not 
    *E300788,4 AMM Remove the link to GL.
    IF !SEEK('DEF')
      *-- Message ' Default link codes have not been setup.  Cannot link to G/L.'
      *-  Buttoon	'OK'
      =gfModalGen('INM00298B00000','DIALOG')
      M_LINK_GL = 'N' 
      M_GL_VERS  = ' '
      M_SYS_DIR  = ' '
      M_GL_CO    = ' '
      M_POST_DET = ' '
      M_DIV_LINK = ' '
      =lfOGShowGet('M_LINK_GL',.T.)
      =lfOGShowGet('M_GL_VERS',.F.)
      =lfOGShowGet('M_SYS_DIR',.F.)
      =lfOGShowGet('M_GL_CO',.F.)
      =lfOGShowGet('M_POST_DET',.F.)
      =lfOGShowGet('M_DIV_LINK',.F.)
      llVldSets = .F.
    ENDIF
  ENDIF
  USE IN (lcTempGL)
ENDIF
*E300788,4 AMM  end

*B601911,1 AMM comment out
*ENDIF  
*B601911,1 AMM end
RETURN llVldSets    

*!*************************************************************
*! Name      : lfvLtoGL
*! Developer : Ahmed Mohammed
*! Date      : 06/16/1997
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen(),lfOGShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvLtoGL()
*!*************************************************************
FUNCTION lfvLtoGL
IF M_LINK_GL = 'N' 
  *-- Message ' Are you sure you want to delete the link to GL'
  *-  Buttoon	'Yes  No'
  lnresp =gfModalGen('INM00270B00006','DIALOG')
  IF lnresp = 1  &&Delete the link to GL
    M_GL_VERS  = ' '
    M_SYS_DIR  = ' '
    M_GL_CO    = ' '
    M_POST_DET = ' '
    M_DIV_LINK = ' '
    * disable the remainder items when deleting the link   
    =lfOGShowGet('M_GL_VERS',.F.)
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
    =lfOGShowGet('M_POST_DET',.F.)
    =lfOGShowGet('M_DIV_LINK',.F.)
  ELSE
    M_LINK_GL  = 'Y'
    =lfOGShowGet('M_LINK_GL',.T.)
  ENDIF
ELSE
  *B601911,1 AMM start, Initialize the GL version to the default value (ARIA) 
  *B601911,1 AMM and get its directory and company.
  M_GL_VERS='A'
  = lfvGLVers(.T.)
  *B601911,1 AMM end
  =lfOGShowGet('M_GL_VERS',.T.)
  *B601911,1 AMM Disable the system directory and company here to wait for 
  *B601911,1 AMM the user to choose the GL link type (ARIA OR SBT OR OTHER)
  *=lfOGShowGet('M_SYS_DIR',.T.)
  *=lfOGShowGet('M_GL_CO',.T.)
  *B601911,1 AMM end
  =lfOGShowGet('M_POST_DET',.T.)
  =lfOGShowGet('M_DIV_LINK',.T.)
ENDIF


*!*************************************************************
*! Name      : lfvGLVers
*! Developer : Ahmed Mohammed
*! Date      : 06/16/1997
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfOGShowGet(),gfModalGen()
*!*************************************************************
*! Parameters: llDefa
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  
*!*************************************************************
FUNCTION lfvGLVers

*B601911,1 AMM start, Parameter to endicate if this function called from its 
*B601911,1 AMM previous setting 'LINK TO GL "Y\N" ' or when validate the setting grid.
PARAMETERS llDefa
*E300867,1 Variable to hold If I opened SYCCOMP OR it is opened before.
PRIVATE llOpen, llDntLnk
llOpen = .F.
*B601911,1 AMM end
llDntLnk = .F.
DO CASE
  CASE M_GL_VERS='S'      && SBT version
    M_SYS_DIR =' '
    M_GL_CO  ='  '
    =lfOGShowGet('M_SYS_DIR',.T.)
    *B601911,1 AMM start, Disable the GL company untill the user choose the directory.
    *=lfOGShowGet('M_GL_CO',.T.)
    =lfOGShowGet('M_GL_CO',.F.)
    *B601911,1 AMM end
  CASE M_GL_VERS='A'    && Aria
    M_SYS_DIR = gcSysHome    
    M_GL_Co = laDATA[1]
    IF !USED('SycComp')
      =gfOpenFile(gcSysHome + 'SycComp',' ','SH')
      *B601911,1 AMM start, when I open the file with myself make the variable TRUE.
      llOpen = .T.
      *B601911,1 AMM  end
    ELSE
      SELECT SycComp    
    ENDIF
    SET ORDER TO TAG cComp_Id
    = SEEK(M_GL_Co)
    IF !Empty(cCompPrnt)
      M_GL_CO  = cCompPrnt
      IF !SEEK(cCompPrnt)
        *=gfDialog('!','The parent compay does not exist, cannot link!!!')
        *B601911,1 AMM start, Display the message only when validate the setting.
        *=gfModalGen('INM00268B00000','DIALOG')
        IF !llDefa
          =gfModalGen('INM00268B00000','DIALOG')
        ENDIF
        *B601911,1 AMM  end
        *M_GL_Co='  '
        *M_SYS_DIR='  '
        *M_GL_VERS=' '
        llDntLnk = .T.
      ELSE
        IF OCCURS('GL',mModlSet) = 0
          *=gfDialog('!','You have to setup The GL module first, cannot link!!!')
          =gfModalGen('INM00267B00000','DIALOG')
          *M_GL_VERS=' '
          *M_SYS_DIR = ' '
          *M_GL_Co = '  '
          llDntLnk = .T.
        ENDIF
      ENDIF
    ELSE
      M_GL_CO = laDATA[1]        
      *=SEEK(M_GL_Co)
      IF OCCURS('GL',mModlSet) = 0
        *=gfDialog('?','You have to setup The GL module first, cannot link!!!')
        *B601911,1 AMM start, Display the message only when validate the setting.
        *=gfModalGen('INM00267B00000','DIALOG')
        IF !llDefa
          =gfModalGen('INM00267B00000','DIALOG')
        ENDIF
        *B601911,1 AMM end
        *M_GL_VERS = ' '
        *M_SYS_DIR = ' '
        *M_GL_Co   = '  '
        llDntLnk = .T.
      ENDIF
    ENDIF
    IF llDntLnk
      M_GL_VERS = ' '
      *E301173,1 AMM Move this part below
      *M_SYS_DIR = ' '
      *M_GL_Co   = '  '
      *E301173,1 AMM end
    ENDIF
    *E301173,1 AMM Empty these two variable not to save them in the data file
    M_SYS_DIR = ' '
    M_GL_Co   = '  '
    *E301173,1 AMM end
    
    =lfOGShowGet('M_GL_VERS',.T.) 
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
    *B601911,1 AMM start, If I opened the file with myself, Close it.
    IF llOpen
      USE IN SYCCOMP
    ENDIF
    *B601911,1 AMM  end
  CASE M_GL_VERS = 'O'   && Other
    M_SYS_DIR    = ' '
    M_GL_CO     = '  '
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
ENDCASE  
*!*************************************************************
*! Name      : lfvSysDir
*! Developer : Ahmed Mohammed
*! Date      : 06/16/1997
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen(),gfOpenFile(),lfOGShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvSysDir()
*!*************************************************************
FUNCTION lfvSysDir

*B601911,1 AMM Make the variable PRIVATE and add a new variable.
PRIVATE llOpen, llReturn, llInvldDir, lcOldPath, lcDir
*B601911,1 AMM variable to indicate if the SYSDATA file is opened by me 
*B601911,1 AMM or opened before.
llOpen = .F.
*B601911,1 AMM  end

llReturn   = .T.
llInvldDir = .F.
lcOldPath  = FULLPATH('')
lcDir      = ALLTRIM(M_SYS_DIR)

ON ERROR llInvldDir = .T.
SET DEFAULT TO &lcDir
ON ERROR
IF EMPTY(lcDir) .OR. llInvldDir
  *- Get the directory.
  lcDir = GETDIR('','SBT System Directory')
  IF EMPTY(lcDir)
    *lcErrMsg = 'The SBT System Directory directory cannot be empty'
    =gfModalGen('INM00250B00000','DIALOG','SBT System Directory')
    llReturn = .F.
  ENDIF 
ENDIF    && Empty or invalid directory
IF llReturn   
  *B602889,1 AMM gfOpenFile needn't the ".dbf" in the name of the file
  *lcSysFile = 'SysData.dbf'
  lcSysFile = 'SysData'
  *B602889,1 AMM end
  SET DEFAULT TO &lcDir
  IF !FILE('SysData.dbf')
    *lcErrMsg = 'The company information file does not exist in the previously specified directory!!! Cannot link with SBT'  
    =gfModalGen('INM00266B00000','DIALOG')
    llReturn=.F.
  ELSE   && Found the file in the given directory
    IF !USED(STRTRA(lcSysFile,'.dbf'))
      *B602889,1 AMM Add '\' only if it'sn't exist in lcDir variable
      *=gfOpenFile(lcDir+'\'+lcSysFile,' ','SH')
      =gfOpenFile(lcDir+IIF(RIGHT(lcDir,1)='\','','\')+lcSysFile,' ','SH')
      *B602889,1 AMM end
      
      *B601911,1 AMM  start, If the file is opened by me, make this variable TRUE.
      llOpen = .T.
      *B601911,1 AMM  end
    ELSE
      SELECT (lcSysFile)
    *B601911,1 AMM Exclude the next lines of code from the else case of the 
    *B601911,1 AMM above IF statement.
    ENDIF
    *B601911,1 AMM end
    lcFrField = 'SYSID'
    IF FIELD(1) <> lcFrField
      *lcErrMsg = 'The company information file does not exist in '+;
      *         'the previously specified directory!!! Cannot link with SBT'
      =gfModalGen('INM00266B00000','DIALOG')
      llReturn = .F.
    ELSE
      M_SYS_DIR = lcDir
      =lfOGShowGet('M_SYS_DIR',.T.)        
    ENDIF  && first field = SYSID
    *B601911,1 AMM terminate the if statement above, to exclude the previous lines of code.
    *ENDIF  && the file Not used
    *B601911,1 AMM  close the SYSDATA table, if I opened with myself.
    IF llOpen
      USE IN SYSDATA
    ENDIF
    *B601911,1 AMM end
  ENDIF    && the file not found
*B601911,1 AMM End the IF statement here and begin a new IF statement.
ENDIF
IF llReturn
  *M_GL_Co='  '
  =lfOGShowGet('M_GL_CO',.T.)
*B601911,1 AMM end
ELSE
  M_SYS_DIR='  '
  M_GL_Co='  '
  =lfOGShowGet('M_SYS_DIR',.T.)
  *B601911,1 AMM start, Disable the GL company if the system directory is not valid.
  *=lfOGShowGet('M_GL_CO',.T.)
  =lfOGShowGet('M_GL_CO',.F.)
  *B601911,1 AMM end
ENDIF  && llreturn

SET DEFAULT TO &lcOldPath
*!*************************************************************
*! Name      : lfvGLCo
*! Developer : Ahmed Mohammed
*! Date      : 06/16/1997
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : gfModalGen(),gfOpenFile()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvGlCo()
*!*************************************************************

FUNCTION lfvGLCo

llReturn  = .T.
lcTmpSysD = ''
IF EMPTY(M_GL_Co)
  *Message = 'Company cannot be empty !!!'
  =gfModalGen('INM00250B00000','DIALOG','Company ')
  llReturn = .F.
ELSE
  lcTmpSysD = GFTEMPNAME()
  SELECT 0
  USE ALLTRIM(M_SYS_DIR)+'\'+'SYSDATA' AGAIN ALIAS &lcTmpSysD
  LOCATE FOR SYSID = "GL" + ALLTRIM(M_GL_Co)
  IF !FOUND()
    *lcInfoMsg = 'Company not found !!!'
    =gfModalGen('INM00269B00000','DIALOG')
    llReturn = .F.
  ELSE  &&FOUND
    *-- Get path for gl data and company name
    M_GL_DIR = SUBSTR(DRIVE,61,30)         && DATA DIRECTORY PATH

    *-- Checking existence of chart of account file
    *-- Check Date base file and Index file.
    lcGL_Co=ALLTRIM(M_GL_Co)
    TmpDbfFile = ALLTRIM(M_GL_DIR)+"GLACNT&lcGL_Co"+".DBF"
    TmpNdxFile = ALLTRIM(M_GL_DIR)+"GLACNT&lcGL_Co"+".CDX"
     
    DO CASE
      *-- Check if selected company was deleted
      CASE SUBSTR(PASS2,1,1) = 'D'
        *lcInfoMsg = 'This company had been deleted. cannot link !!!'
        =gfModalGen('INM00265B00000','DIALOG')
        llReturn = .F.
      *-- Check if the entered company is consolodated for SBT
      CASE ((M_GL_VERS = 'S') .AND. (SUBSTR(STR5,10,1) = 'Y'))
        *lcInfoMsg = 'This company for consolidation only. cannot link !!!'
        =gfModalGen('INM00264B00000','DIALOG')
        llReturn = .F.
      CASE !FILE(TmpDbfFile) .OR. !FILE(TmpNdxFile)
        *lcInfoMsg = 'Chart of account file for this company not found !!!'
        =gfModalGen('INM00263B00000','DIALOG')
        llReturn = .F.
    ENDCASE
    
  ENDIF   &&!FOUND
ENDIF    && Not empty

IF !llReturn
  *= gfDialog('I',lcInfoMsg)
  M_GL_Co = '  '
  =lfOGShowGet('M_GL_Co',.T.)
ENDIF

*B601911,1 AMM start, If the user changed the system directory , so we have to open 
*B601911,1 AMM the other sysdata file, so close this.
IF USED (lcTmpSysD)
  USE IN (lcTmpSysD)
ENDIF

*B601911,1 AMM end

*!*************************************************************
*! Name        : lfSetGLMsk
*! Developer   : Reham Al-Allamy
*! Date        : 08/07/1997
*! Purpose     : Function returned with the account mask to 
*!             : validate cAdjAcct from the related field 
*!             : screen that branches from the codes screen.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Called From : SMCODES.PRG
*!*************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : lnReturn (GL Account Mask)
*!*************************************************************
*! Example     : lfSetGLMsk(laComp[lnComp,2])
*!*************************************************************
*! Modification: *E300459,1 Added the function.
*!*************************************************************
*
FUNCTION lfSetGLMsk
PARAMETERS lcActComp
PRIVATE lnAlias , lcActComp

*-- Save the current alias.
lnAlias    = ALIAS()
*-- Var. hold if there is GL Link or not in the popup active company.
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',lcActComp)) = 'Y')
*-- Var. hold link GL WITH ?? (Aria - SBT - Other)
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',lcActComp)))
*-- Define the mask variable
lcAcMask   = SPACE(0)

*-- Validate only if there is GL Link.
IF llGlLink
  *-- If link with Aria or Other.
  IF lcLinkWith $ "AO"
    *-- Open the company file to get the path of the popup active company.
    USE (gcSyshome + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    =SEEK(lcActComp, "CompFile")
    *E301098,1 Hesham (Start)
*    lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    *E301098,1 Hesham (End)    
    *-- If the popup active company has a parent company.
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      =SEEK(lcPrntComp, "CompFile")
    *E301098,1 Hesham (Start)      
      *lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
      lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    *E301098,1 Hesham (End)      
    ENDIF
    USE IN CompFile
    *-- Open the account code strucure file.
    USE (lcPthToUse + "ACCOD") IN 0 AGAIN ALIAS CodeStr ORDER AccSegNo
    SELECT CodeStr
    GOTO TOP
    lcRep     = IIF(lcLinkWith = "A", "9", "X")
    *-- Fill the mask variable.
    lcAcMask  = "X" + SUBSTR(STRTRAN(ALLTRIM(cAcsMask),"#",lcRep),2)
    USE IN CodeStr
  ELSE
    *-- Get the SBT company that has GL Link.
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , lcActComp))
    *-- Get the path for the data file.
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', lcActComp))
    *-- Account code strucure file name.
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    lcAcMask   = SPACE(0)
    *-- Open the account code strucure file.
    USE (lcAcntStrc) IN 0 AGAIN ALIAS AcntStrc ORDER SegID
    SELECT AcntStrc
    *-- Fill the mask variable.
    SCAN FOR SegLen > 0
      lcAcMask = lcAcMask + IIF(EMPTY(lcAcMask),"","-") + ALLTRIM(SegMask)
    ENDSCAN
    USE IN AcntStrc
  ENDIF
ENDIF

*-- Restore the alias.
SELECT (lnAlias)

*-- Return with the mask variable.
RETURN lcAcMask

*!*************************************************************
*! Name        : lfvAdjAcct
*! Developer   : Reham Al-Allamy
*! Date        : 08/07/1997
*! Purpose     : Function to validate cAdjAcct from the related 
*!             : field screen that branches from the codes screen.
*!*************************************************************
*! Calls       : gfVldFld()
*!*************************************************************
*! Called From : mVald_Str in SydField
*!*************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : lnReturn (GL Account)
*!*************************************************************
*! Example     : lfvAdjAcct()
*!*************************************************************
*! Modification: *E300459,1 Added the function.
*!*************************************************************
*
FUNCTION lfvAdjAcct
PARAMETERS lcCurVal , lcLgVarNam , lcActComp , lcOldVal
PRIVATE lcCurVar, lcCurVal  , lcEmpty , llEmpty, laTemp, ;
PRIVATE lcToGet, lcBrFields, lcFile_Ttl

*-- Save the current alias.
*E300930,1 AMM More effecient
*lnAlias    = ALIAS()
lnAlias    = SELECT(0)
*E300930,1 AMM end
lcCurVar   = SYS(18)
lcCurVal   = EVAL(lcCurVar)
*-- Var. hold if there is GL Link or not in the popup active company.
llGlLink   = (ALLTRIM(gfGetMemVar('M_LINK_GL',lcActComp)) = 'Y')
*-- Var. hold link GL WITH ?? (Aria - SBT - Other)
lcLinkWith = (ALLTRIM(gfGetMemVar('M_GL_VERS',lcActComp)))

*-- Validate only if there is GL Link.
*B605423,1 RAE (sTART)
*IF llGlLink 
IF llGlLink AND !EMPTY(lcCurVal)
*B605423,1 RAE (end)
  *-- If link with Aria or Other.
  IF lcLinkWith $ "AO"
    *-- Open the company file to get the path of the popup active company.
    USE (gcSyshome + "SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
    =SEEK(lcActComp, "CompFile")
    *E301098,1 Hesham (Start)
*    lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
    lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
    *E301098,1 Hesham (End)
    *-- If the popup active company has a parent company.
    IF !EMPTY(CompFile.cCompPrnt)
      lcPrntComp = CompFile.cCompPrnt
      =SEEK(lcPrntComp, "CompFile")
      *E301098,1 Hesham (Start)
      *lcPthToUse = ALLTRIM(CompFile.cCom_DDir)
      lcPthToUse = gfGetDataDir(ALLTRIM(CompFile.cCom_DDir))
      *E301098,1 Hesham (End)
    ENDIF
    USE IN CompFile
    *-- Open the chart of accounts file.
    IF lcLinkWith = "A" AND !USED('lcLinkChar')
      *B602556,1 If the file is already in use in alias GLACCHAR, it gives
      *B602556,1 a 'File is in use' message. 
      *USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE ALIAS lcLinkChar
      USE (lcPthToUse + "GLACCHAR") IN 0 ORDER ACCTCODE AGAIN ALIAS lcLinkChar
      *B602556,1 end
    ENDIF
    lcAcntBrwF = "cAcctCode:24:H='Account Code',cAccNlDes:65:H='Description'"
    lcAcntFld  = "cAcctCode"
    lcAcntDesF = "cAccNlDes"
  ELSE
    *-- Get the SBT company linked to the popup active comany.
    lcLinkComp = ALLTRIM(gfGetMemVar('M_GL_CO'  , lcActComp))
    *-- Get the directory of the SBT company linked to the popup active company.
    lcSBTGLDir = ALLTRIM(gfGetMemVar('M_SYS_DIR', lcActComp))
    *-- Var. hold the chart of account name & path.
    lcAcntChrt = lcSBTGLDir + "\GLDATA\GLACNT" + lcLinkComp + ".DBF"
    *-- Var. hold the account code strucure name & path.
    lcAcntStrc = lcSBTGLDir + "\GLDATA\GLSTRU" + lcLinkComp + ".DBF"
    *-- Open the chart of accounts file.
    IF !USED("lcLinkChar")
      *B602556,1 If the file is already in use in alias GLACCHAR, it gives
      *B602556,1 a 'File is in use' message. 
      *USE (lcAcntChrt) IN 0 ALIAS lcLinkChar ORDER GlAcnt
      USE (lcAcntChrt) IN 0 AGAIN ALIAS lcLinkChar ORDER GlAcnt
      *B602556,1 end
    ENDIF
    
    lcAcntBrwF = "glAcnt:24:H='Account Code',glDesc:53:H='Description'"
    lcAcntFld  = "glAcnt"
    lcAcntDesF = "glDesc"
  ENDIF
  
  *-- If the company not linked to Other.
  IF lcLinkWith <> "O"
    *-- If the value enetered does not exist in the chart 
    *-- of accounts file, call the browse.
    IF !SEEK(lcCurVal, "lcLinkChar")
      SELECT lcLinkChar
      DIMENSION laTemp[2]
      laTemp     = SPACE(0)
      lcBrFields = lcAcntBrwF
      lcFile_Ttl = "Chart of Accounts"
      lcToGet    = lcAcntFld + "," + lcAcntDesF
      IF gfBrows(.F.,lcToGet,'laTemp')
        &lcLgVarNam = .T.
        &lcCurVar   = laTemp[1]
      ELSE
        *E300930,1 AMM comment to don't return .F.
        *&lcLgVarNam = IIF(EMPTY(lcOldVal) , .F. , .T.)
        *E300930,1 AMM end
        &lcCurVar   = lcOldVal
        *E300930,1 AMM comment to don't return to the same object
        *_CUROBJ     = _CUROBJ
        *E300930,1 AMM end
      ENDIF
    ELSE
      &lcLgVarNam = .T.
    ENDIF  
    USE IN lcLinkChar
  ELSE
    &lcLgVarNam = .T.
  ENDIF
ELSE
  &lcLgVarNam = .T.
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvTaxSt
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Valid function of the Tax status option in the setting grid
*!             If the Tax status is YES enable the tax choices and vice versa.
*! REF       : *B601911,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfOGShowGet()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvTaxSt()
*!*************************************************************
FUNCTION lfvTaxSt
*B601911,1 If tax = Yes
IF M_TAX = 'Y'
  M_TAX_RATE = 1.0
  =lfOGShowGet('M_TAX_DESC',.T.)    
  =lfOGShowGet('M_TAX_METH',.T.)        
  =lfOGShowGet('M_TAX_RATE',.T.)    
  =lfOGShowGet('M_TAX_REFE',.T.)    
  *C102212,1 (Begin) Enable it.
  =lfOGShowGet('M_HST_RATE',.T.)
  *C102212,1 (End)
ELSE
  M_TAX_DESC = SPACE(20)
  M_TAX_METH = SPACE(1)
  M_TAX_RATE = 0.0
  M_TAX_REFE = SPACE(30)
  =lfOGShowGet('M_TAX_DESC',.F.)    
  =lfOGShowGet('M_TAX_METH',.F.)        
  =lfOGShowGet('M_TAX_RATE',.F.)    
  =lfOGShowGet('M_TAX_REFE',.F.)    
  *C102212,1 (Begin) Disanable it.
  =lfOGShowGet('M_HST_RATE',.F.)
  *C102212,1 (End)
ENDIF
*C102212,1 (Begin) Disanable it is any country rather CANADA.
IF UPPER(ALLTRIM(gcContCode))<>'CANADA'
  =lfOGShowGet('M_HST_RATE',.F.)
ENDIF  
*C102212,1 (End)

*!*************************************************************
*! Name      : lfvfnTaxR
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Valid function of tax rate
*! REF       :  *E300867,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvfnTaxR()
*!*************************************************************
FUNCTION lfvfnTaxR
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
IF &lcVar < 0
  =gfModalGen('INM00234B00000','DIALOG','Tax Rate')
  &lcVar    = 0
  &lcReturn = .F.
ENDIF

*!*************************************************************
*! Name      : lfvlnkCod
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of link code related field
*! REF       :  *E300930,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcVar , lcReturn , lcComp , lcOldRltd
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvlnkCod()
*!*************************************************************
FUNCTION lfvlnkCod
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
PRIVATE lcGLODIR
lcGLODIR = ''

IF !EMPTY(EVAL(lcVar)) .AND. gfGetMemVar("M_LINK_GL", lcComp) = "Y" .AND. gfGetMemVar("M_DIV_LINK", lcComp)='Y'
  *B602589,1 AMM Initialize
  llOpGl = .F.
  *B602589,1 AMM end
  *E300930,1 AMM Open the GL_LINK file of the required company
  IF !USED('GL_LINK')
    *B602589,1 AMM set the variable to close the file if opened
    *=gfOpenFile(ALLTRM(laComp[lnComp,3])+'GL_LINK','GL_LINK','SH')
    llOpGl = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'GL_LINK','GL_LINK','SH')
    *B602589,1 AMM end
  ELSE
   IF DBF('GL_LINK') # UPPER(ALLTRIM(laComp[lnComp,3])+'GL_LINK.DBF')
     lcGLODIR = DBF('GL_LINK')
     USE IN GL_LINK
     *B602589,1 AMM adjust the spelling mistake in ALLTRIM
     *=gfOpenFile(ALLTRM(laComp[lnComp,3])+'GL_LINK','GL_LINK','SH')
     =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'GL_LINK','GL_LINK','SH')
     *B602589,1 AMM end
    ELSE
     SELECT GL_LINK
     SET ORDER TO TAG GL_LINK
    ENDIF
  ENDIF
  IF !SEEK(EVAL(lcVar),'GL_LINK')
    lcVal = SPACE(0)
    lcVal = EVAL(lcVar)
    *E300930,1 AMM Browse customer types only.
    =gfGlBrowse('01',@lcVal)
    &lcVar = lcVal
  ENDIF
  IF !EMPTY(lcGLODIR)
    =gfOpenFile(lcGLODIR,'','SH')
  ENDIF
  *B602589,1 AMM Close the file
  IF USED('GL_LINK') .AND. llOpGl
    =gfCloseFile('GL_LINK')
  ENDIF
  *B602589,1 AMM end
ENDIF
*!*************************************************************
*! Name      : lfvlnkSls
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of sales link code related field
*! REF       :  *E300930,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcVar , lcReturn , lcComp , lcOldRltd
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvlnkSls()
*!*************************************************************

FUNCTION lfvlnkSls
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
PRIVATE lcGlODir
lcGLODIR = ''
*E300930,1 AMM If link to GL & link code at division level = "Y"
IF !EMPTY(EVAL(lcVar)) .AND. gfGetMemVar("M_LINK_GL", lcComp) = "Y" ;
                     .AND. gfGetMemVar("M_DIV_LINK", lcComp)='Y'
  *B602589,1 AMM Initialize
  llOpGl = .F.
  *B602589,1 AMM end
  IF !USED('GL_LINK')
    *B602589,1 AMM Set the variable to close the file if opened
    *=gfOpenFile(ALLTRM(laComp[lnComp,3])+'GL_LINK1','GL_LINK1','SH')
    llOpGl = gfOpenFile(ALLTRIM(laComp[lnComp,3])+'GL_LINK','GL_LINK1','SH')
    *B602589,1 AMM end
  ELSE
    IF DBF('GL_LINK') # UPPER(ALLTRIM(laComp[lnComp,3])+'GL_LINK.DBF')
      lcGLODIR = DBF('GL_LINK')
      USE IN GL_LINK
      *B602589,1 AMM Fix the spelling mistake
      *=gfOpenFile(ALLTRM(laComp[lnComp,3])+'GL_LINK1','GL_LINK1','SH')
      =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'GL_LINK','GL_LINK1','SH')
      *B602589,1 AMM end
    ELSE
      SELECT GL_LINK
      SET ORDER TO TAG GL_LINK1
    ENDIF
  ENDIF
 
  IF !SEEK('02'+EVAL(lcVar),'GL_LINK')
    lcVal = EVAL(lcVar)
    =gfGLBrowse('02',@lcVal,'',1)
    &lcVar = lcVal
  ENDIF
  IF !EMPTY(lcGLODIR)
    =gfOpenFile(lcGLODIR,'','SH')
  ENDIF
  *B602589,1 AMM Close the file
  IF USED('GL_LINK') .AND. llOpGl
    =gfCloseFile('GL_LINK')
  ENDIF
  *B602589,1 AMM end
ENDIF


*!*************************************************************
*! Name      : lfvGlMand
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Conditional function of the Field GLACCOUNT to be 
*!             mandatory or not
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: lnCode
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvGlMand()
*!*************************************************************
FUNCTION lfvGlMand
PARAMETERS lcRet
&lcRet = gfGetMemVar("M_LINK_GL", laComp[lnComp,2]) = 'Y'

*!*************************************************************
*! Name      : lfvDivGrp
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of division group related field
*! REF       : *E300930,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcVar , lcReturn , lcComp , lcOldRltd
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvDivGrp()
*!*************************************************************
FUNCTION lfvDivGrp
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
PRIVATE lcDivGrp , lcDataDir

*B603925,1 HBG Get the Data Directry In case of no active company selected [Begin]
lcDataDir  = ALLTRIM(laComp[lnComp,3])
*B603925,1 [End]

lcDivGrp = gfTempName()
*E300930,1 AMM If link to GL & link code at division level = "Y"
IF !EMPTY(EVAL(lcVar)) .AND. gfGetMemVar("M_DIV_SEQ", laComp[lnComp,2]) = 'Y' 
  
  *B602589,1 AMM Open the file
  *B603925,1 HBG Get the Data Directry In case of no active company selected [Begin]
  *llOpSeq = gfOpenFile(gcDataDir+'SEQUENCE','','SH')
  llOpSeq = gfOpenFile(lcDataDir+'SEQUENCE','','SH')
  *B603925,1 HBG Get the Data Directry In case of no active company selected [End  ]
  *B602589,1 AMM end
  
  *-- Collect all division group data from sequence file.
  SELECT DISTINCT cseq_group;
   FROM SEQUENCE Seq_file   ;
   WHERE !EMPTY(cseq_group)   ;
   ORDER BY cseq_group      ;
   INTO CURSOR (lcDivGrp)

  lcToGet = EVAL(VARREAD())
  LOCATE FOR cseq_group = lcToGet
  IF !FOUND()
    lcBrFields     =  "cseq_group :H='Group'"
    laTemp = gfTempName()
    DIMENSION laTemp[1]
    STORE SPACE(0) TO laTemp
    SELECT (lcDivGrp)
    =gfBrows(.F.,"cseq_group",'laTemp')
    &lcVar = laTemp[1]
  ENDIF
  *B602589,1 AMM Close file
  IF USED('SEQUENCE') .AND. llOpSeq
    =gfCloseFile('SEQUENCE')
  ENDIF
  *B602589,1 AMM end
ENDIF
*-- end of lfvDivGrp.

*!*************************************************************
*! Name      : lfvfnRylR
*! Developer : Ahmed Mohammed
*! Date      : 02/12/1998
*! Purpose   : Valid function of tax rate
*! REF       :  *E300867,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvfnRylR()
*!*************************************************************
FUNCTION lfvfnRylR
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
IF &lcVar < 0
  =gfModalGen('INM00234B00000','DIALOG','Royalty Rate')
  &lcVar    = 0
  &lcReturn = .F.
ENDIF


*!**************************************************************************
*! Name      : lfvMFGOpr
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : Valid function of the MFG operation code, 
*!             (Contractor Code, lead time, In House ) related fields
*! REF       : *E300930,1 AMM
*! Notes     : This function moved from the MF.PRG to SM.PRG due to *B801900,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : gfOpenFile(), gfApVnBrow(), gfModalGen(), lfRLShowGet()
*!**************************************************************************
*! Parameters: lcCurVal , lcRet , lcCompID, lcOldRltd
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfvMFGOpr()
*!**************************************************************************
FUNCTION lfvMFGOpr
PARAMETERS lcCurVal , lcRet , lcCompID, lcOldRltd

lcFldCod = laRelFld[lnAryNo,1]
lcCurVal = EVAL(lcCurVal)
DO CASE
  *B602626,1 AMM Do validations if the file exist.
  *CASE lcFldCod = 'CCONTCODE' 
  CASE lcFldCod = 'CCONTCODE' .AND. FILE(ALLTRIM(laComp[lnComp,3])+'APVENDOR.DBF')
  *B602626,1 AMM end
    IF !USED('APVENDOR')
      =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APVENDOR','VenCode','SH')
    ELSE
      IF DBF('APVENDOR') # ALLTRIM(laComp[lnComp,3])+'APVENDOR.DBF'
        USE IN ('APVENDOR')
        =gfOpenFile(ALLTRIM(laComp[lnComp,3])+'APVENDOR','VenCode','SH')      
      ENDIF
    ENDIF
    *E300930,1 AMM If IN HOUSE = Yes
    *E300930,1 AMM laCodInfo is an array hold data of POPUPs in the related field 
    *E300930,1 AMM screen, Each POPUP has a temporary name
    
    *E300930,1 AMM Get the position of the INHOUSE POPUP
    lnPos = ASUBSCRIPT(laRelFld,ASCAN(laCodInfo,'LINHOUSE',1),1 )
    
    IF EVAL(laCodInfo[lnPos,3]) = 2
      lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
      SELECT APVENDOR
      SET ORDER TO TAG VenCode 
      *E300930,1 AMM IF Statment to check if we are going to Browse
      IF !EMPTY(lcCurVal)
        lnPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CCONTNAME',1),1 )
        IF !('?' $ lcCurVal ) .AND. SEEK(lcCurVal , 'APVENDOR')
          laRelFld[lnPos,6] = APVENDOR.cVenComp
        ELSE
          *-- HDM E301268,1 [Start] Filter vendor browse according to Supp. Type = 'C' --> Contractor
          *=gfApVnBrow(@lcCurVal)
          =gfApVnBrow(@lcCurVal,.F.,'C')
          *-- HDM E301268,1 [End]
          IF !EMPTY(lcCurVal)
            &lcVar = lcCurVal      && Update the field
            laRelFld[lnPos,6] = APVENDOR.cVenComp
          ENDIF
        ENDIF
        =lfRLShowGet('CCONTNAME',.F.)
      ENDIF
    ELSE
     =lfRLShowGet('CCONTNAME',.T.)
    ENDIF
    USE IN APVENDOR

  CASE lcFldCod = 'LEADTIME'
    *E300930,1 AMM lead time must be greater than zero
    IF EVAL(VARREAD()) <= 0
      =gfModalGen('INM00234B00000','DIALOG','Lead time')
      _CUROBJ = _CUROBJ
    ENDIF
  CASE lcFldCod = 'LINHOUSE'
    *E300930,1 AMM If the user changed this field , initialize the 
    *E300930,1 AMM contractor code and name.
    lnPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CCONTCODE',1),1 )
    laRelFld[lnPos,6] = SPACE(8)
    lnPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'CCONTNAME',1),1 )
    laRelFld[lnPos,6] = SPACE(30)    
    =lfRLShowGet('CCONTCODE',.T.)
    =lfRLShowGet('CCONTNAME',IIF(EVAL(VARREAD()) = 2,.F.,.T.) )
 
ENDCASE


*!**************************************************************************
*! Name      : lfRLShowGet
*! Developer : Ahmed Mohammed
*! Date      : 11/04/1998
*! Purpose   : to refresh any object in the related fields screen
*! REF       : *E300930,1 AMM
*! Notes     : This function moved from the MF.PRG to SM.PRG due to *B801900,1
*!**************************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!**************************************************************************
*! Parameters: lcRelVar,llStat
*!                 lcRelVar variable to be refreshed
*!                 llStat   status (Enabled or Disabled)
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Example   :  =lfRLShowGet()
*!**************************************************************************
FUNCTION lfRLShowGet

PARAMETERS lcRelVar,llStat
lcObj = SPACE(0)
IF ASCAN(laRelFld,lcRelVar,1) # 0
  lnPos = ASUBSCRIPT(laRelFld, ASCAN(laRelFld,lcRelVar,1) , 1 )
  *E300930,1 AMM If the variable is in POPUP
  IF laRelFld[lnPos,3] = 'L' .OR. !EMPTY(laRelFld[lnPos,12])
    IF ASCAN(laCodInfo , lcRelVar, 1) # 0
      lnPos1 = ASUBSCRIPT(laCodInfo,ASCAN(laCodInfo , lcRelVar, 1) ,1 )
      lcObj = laCodInfo[lnPos1,3]
    ENDIF
  ELSE && i.e. normal get field
    lcObj = "laRelFld[" + ALLTRIM(STR(lnPos)) + ",6]"
  ENDIF
  IF !EMPTY(lcObj)
    IF llStat
      SHOW GET &lcObj ENABLE
    ELSE
      SHOW GET &lcObj DISABLE
    ENDIF
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvEOMDay
*! Developer : Ahmed Mohammed
*! Date      : 05/09/1999
*! Purpose   : Valid function of EOM day (related field to payment terms code)
*! REF       : *E301217,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvEOMDay()
*!*************************************************************
FUNCTION lfvEOMDay
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd
*B604470,1 NAD  05/22/2001 (Start) 
PRIVATE lnFPos
lnFPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'EOM',1),1 )
lnFPos=ASCAN(laCodInfo,'EOM') 
IF EVAL(laCodInfo[lnFPos,3]) =2
*B604470,1 NAD  05/22/2001 (End)
  IF &lcVar > 31 .OR. &lcVar <= 0
    *-- Message End Of Month day should be between 1 & 31
    =gfModalGen('INM00351B00000','DIALOG','End Of Month day|1|31')
    &lcVar    = lcOldRltd
    &lcReturn = .F.
  ENDIF
*B604470,1 NAD  05/22/2001 (Start) 
ELSE
  &lcVar  = 0
  =lfRLShowGet('EOMDAY',&lcVar  # 0)
ENDIF
*B604470,1 NAD  05/22/2001 (End) 

*!*************************************************************
*! Name      : lfvEOM
*! Developer : Ahmed Mohammed
*! Date      : 05/09/1999
*! Purpose   : Valid function of EOM (related field to payment terms code)
*! REF       : *E301217,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvEOM()
*!*************************************************************
FUNCTION lfvEOM
PARAMETER lcVar , lcReturn , lcComp , lcOldRltd

*-- If EOM setting is YES enable EOMDAY field, else disable

lnPos = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,'EOMDAY',1),1 )
laRelFld[lnPos,6] = IIF(EVAL(SYS(18)) = 2,21,0)
lcEOMVal=IIF(EVAL(SYS(18)) = 2,'Y','N')
=lfRLShowGet('EOMDAY',EVAL(SYS(18)) = 2)




*!*************************************************************
*! Name      : lfvLnkCDiv
*! Developer : Ahmed Mohammed
*! Date      : 05/09/1999
*! Purpose   : Valid function of Link Code At Division Level setting in company setup
*! REF       : *B603098,1 AMM
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvLnkCDiv()
*!*************************************************************
FUNCTION lfvLnkCDiv
PRIVATE lcSequence

*B603098,1 AMM If user changed setting from YES to NO
IF M_DIV_SEQ = 'N'
  *B603098,1 AMM Open the sequence file
  lcSequence = gfTempName()
  =gfOpenFile(gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))+'SEQUENCE','','SH',@lcSequence,.T.)
  *B603098,1 AMM Check if any division group intered.
  LOCATE FOR !EMPTY(cseq_group)
  *B603098,1 AMM Don't allow changing if at least one division entered
  IF FOUND()
    *Message : Cannot change.  One or more division groups has already been entered.
    =gfModalGen("INM003540B00000","DIALOG")
    M_DIV_SEQ = 'Y'
    =lfOGShowGet('M_DIV_SEQ',.T.)
  ENDIF
  *B603098,1 AMM Close sequence file
  IF USED(lcSequence)
    =gfCloseFile(lcSequence)
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfSeqVld
*! Developer : (WA) Walid Abo El-Magd
*! Date      : 05/09/1999
*! Purpose   : Force entering value to Operation Sequence if Consider as operation is "YES".
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : 
*!*************************************************************
*!B802700,1
FUNCTION lfSeqVld
PARAMETERS llToret
&llToret. = .F.
PRIVATE lnConsPos,lnSeqPos,lnSeqRow,lcOldSet
lnSeqPos   = 0
lcOldSet   = SET('EXACT')
SET EXACT OFF
lnConsPos  = ASCAN(laRelFld,'LMFGOPR')  &&-- Consider as operation element number
IF lnConsPos > 0
  lnPos      = ASCAN(laCodInfo,'LMFGOPR') 
  lcArryName = laCodInfo[lnPos+1]
  lcArryValu = laCodInfo[lnPos+2]
ENDIF

lnSeqPos = ASCAN(laRelFld,'COPERSEQ')  &&-- Element number
IF lnSeqPos > 0
  lnSeqRow  = ASUBSCRIPT(laRelFld,lnSeqPos,1) &&--	Row number 
  &llToret. = &lcArryName[&lcArryValu,2] = 'Y' 
ENDIF
SET EXACT &lcOldSet

*!*************************************************************
*! Name      : lfLnkModul
*! Developer : WAB - Walid A. Wahab
*! Date      : 01/05/2000
*! Purpose   : check if there are installed modules  can be linked with Gl
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfLnkModul()
*!*************************************************************
*B802792,1 
*!*************************************************************
FUNCTION lfLnkModul
PRIVATE llModFound,lnAlias
*---llModFound---> hold .T. if there any module can linked to GL
*---lnAlias -----> hold current alias
lnAlias= SELECT()

*B802792,1 Reham  on 04/24/2000 [Start]
*llModFound = ('IC' $ gcCmpModules .OR. 'AR' $ gcCmpModules .OR. 'MA' $ gcCmpModules .OR. ;
              'PO' $ gcCmpModules .OR. 'PS' $ gcCmpModules .OR. 'RM' $ gcCmpModules .OR. ;
              'MF' $ gcCmpModules )
*B802792,1 Check the modules of the current company in the company screen not the active running company.
llModFound = ('IC' $ ALLTRIM(laData[15]) OR 'AR' $ ALLTRIM(laData[15]) OR 'MA' $ ALLTRIM(laData[15]) OR ;
              'PO' $ ALLTRIM(laData[15]) OR 'PS' $ ALLTRIM(laData[15]) OR 'RM' $ ALLTRIM(laData[15]) OR ;
              'MF' $ ALLTRIM(laData[15]) )
*B802792,1 Reham  on 04/24/2000 [End]

*---- IF there is no modules can linked to gl check in ap setup if the ap module 
*---- is linked to gl to default the option 'Link to Gl' -->(yes) if it is linked
*B804136,1 SSH Fix APSETUP not found.[Start]
*IF !llModFound
IF !llModFound  .AND. FILE(ALLTRIM(laData[11])+"APSETUP.DBF")
*B804136,1 SSH Fix APSETUP not found.[End]
  IF !USED('APSETUP')
    *B802792,1 Reham  on 04/24/2000 [Start]
    *B802792,1 Open the ApSetup file for the current company in the company screen not the active company.
    *=gfOpenFile(gcDataDir+"APSETUP",'', "SH",'',.T.)
    =gfOpenFile(ALLTRIM(laData[11])+"APSETUP",'', "SH",'',.T.)
    *B802792,1 Reham  on 04/24/2000 [End]
  ENDIF
  M_LINK_GL = APSETUP.cApsGllink 
ENDIF  
SELECT (lnAlias)
RETURN llModFound

*!*************************************************************
*! Name      : lfvUccMan
*! Developer : Nader Anis NAD
*! Date      : 02/16/2000
*! Purpose   : Check if the UPC manufacturer code exist in the
*!           : StyleUpc file 
*! Refer to  : B603468,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvUccMan()
*!*************************************************************
FUNCTION lfvUccMan
PARAMETERS lcVar , lcReturn , lcComp , lcOldRltd 
IF &lcVar <> lcOldRltd
  *B804136,1 SSH Fix APSETUP not found.[Start]
  *IF !USED('StyleUpc') 
  IF !USED('StyleUpc') .AND. FILE(gcDataDir+"STYLEUPC")
  *B804136,1 SSH Fix APSETUP not found.[END]
    =gfOpenFile(gcDataDir+"StyleUpc",'StyUpcN', "SH",'',.T.)
  ENDIF
  IF SEEK (lcOldRltd,'StyleUpc') 
    =gfModalGen("TRM000000B00000","DIALOG",'','','UPCs have been generated for this UCC manufacturer ID. Cannot modify.')
    &lcvar=lcOldRltd
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfChkUcc
*! Developer : Nader Anis NAD
*! Date      : 02/22/2000
*! Purpose   : Check if the UPC manufacturer code exist in the
*!           : StyleUpc file  
*! Refer to  : B603468,1
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfChkUcc()
*!*************************************************************
FUNCTION lfChkUcc

*B804136,1 SSH Fix StyleUPC not found.[Start]
PRIVATE lnToRetUc,lcTempDir
lcTempDir  = gfGetDataDir(ALLTRIM(laData[11]))
lnToRetUc = .F.

*B605279,1 WAB (Start) - check the file with the extension.
*IF FILE(lcTempDir+"STYLEUPC")
IF FILE(lcTempDir+"STYLEUPC.DBF")
*B605279,1 WAB (End)

*B804136,1 SSH Fix StyleUPC not found.[END]
  IF !USED('StyleUpc')
    *B603744,1 (Begin) In case of no active company selected, gcDataDir is empty generating the bug.
    lcOldDataD = gcDataDir
    gcDataDir  = gfGetDataDir(ALLTRIM(laData[11]))
    *B603744,1 (End)
    =gfOpenFile(gcDataDir+"StyleUpc",'StyUpcN', "SH",'',.T.)
    *B603744,1 (Begin) Restore the old data dir
    gcDataDir  = lcOldDataD
    *B603744,1 (End)
  ENDIF
  *B804136,1 SSH Fix StyleUPC not found.[Start]
  *RETURN !SEEK (XManufId,'StyleUpc')
  lnToRetUc = !SEEK (XManufId,'StyleUpc')
ENDIF
RETURN(lnToRetUc)
*B804136,1 SSH Fix StyleUPC not found.[END]

*TTTT
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
 M_ENTMETH  = ' '
 M_ACTNTYPE = ' '
 * disable the remainder items when deleting the link   
 =lfOGShowGet('M_ATOMDIR',.F.)
 =lfOGShowGet('M_ENTMETH',.F.)
 =lfOGShowGet('M_ACTNTYPE',.F.)
ELSE
 =lfOGShowGet('M_ATOMDIR',.T.)
 =lfOGShowGet('M_ENTMETH',.T.)
 =lfOGShowGet('M_ACTNTYPE',.T.)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvAtomDir
*! Developer : Timour Abdalla
*! Date      : 09/17/00
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
FUNCTION lfvAtomDir

M_ATOMDIR = GETDIR('','Atomic Software Directory')
IF EMPTY(M_ATOMDIR)
  M_USEATOM = 'N'
  =lfvAtomInt()
ELSE
  M_ATOMDIR = ALLT(M_ATOMDIR)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvHistCmp
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2000
*! Purpose   : Valid function of history company code.
*! Reference : *E301469,1 AMH
*!*************************************************************
*! Called from : SMCMSET.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvHistCmp()
*!*************************************************************

FUNCTION lfvHistCmp
IF llHist .AND. !EMPTY(M_Comp_Id)
  IF gcAct_Comp == M_Comp_Id
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "The active company cannot be the current one.")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF M_Comp_Id == laData[1]
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "This is a History Company,"+;
                "select another company to be the Active Company.")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  lnAlias = SELECT(0)
  SELECT SYCCOMP
  lnRec = RECNO()
  SEEK laData[1]
  lcTrgDir = ALLTRIM(CCOM_DDIR)
  IF SEEK(M_Comp_Id)
    IF !GfGetMemVar('LLHIST',M_Comp_Id) .AND. EMPTY(GfGetMemVar('M_Comp_Id',M_Comp_Id))
      lcMsg = "You are going to make the company " + laData[1] + " a history company for " +;
              "company " + M_COMP_ID + " . This will remove all data in company " + laData[1] +;
              " ! Are you sure you want to continue ?"
      IF gfModalGen("TRM00000B00006","DIALOG","Company Information",.F.,lcMsg) = 2
        LLHIST = .F.
        M_COMP_ID = ''
        *B604095,1 AMH 01/08/2001 Enable all Setups before lfOGRShow [Start]
        lnI = 1
        FOR lnI = 1 TO ALEN(laOGObjTyp,1)
          =lfOGShowGet(laOGObjTyp[lnI,1],.T.)
        ENDFOR
        *B604095,1 AMH [End]
        =lfOGRShow()
        IF lnRec > 0 .AND. lnRec <= RECCOUNT()
          GOTO RECORD lnRec
        ENDIF
        SELECT (lnAlias)
        RETURN
      ENDIF
      lcSorDir = ALLTRIM(CCOM_DDIR)
      lcCompCod = laData[1]
      SCATTER FIELDS &lcScFields MEMO TO laData
      laData[1] = lcCompCod
      laData[2] = 'History of ' + laData[2]
      laData[11] = lcTrgDir
      laData[15] = STRTRAN(laData[15],'GL|','')
      laData[15] = STRTRAN(laData[15],'GL,','')
      laData[15] = STRTRAN(laData[15],'GL','')
      lcCurModul = STRTRAN(MMODLSET,'GL|','')
      lcCurModul = STRTRAN(lcCurModul,'GL,','')
      lcCurModul = STRTRAN(lcCurModul,'GL','')
      SEEK laData[1]
      REPLACE mModlSet  WITH lcCurModul
      *-- copy modules files.
      IF USED('SETUPS')
        USE IN SETUPS
      ENDIF
      IF USED('ACCOD')
        USE IN ACCOD
      ENDIF
      IF USED('FISHD')
        USE IN FISHD
      ENDIF
      IF USED('FSHLD')
        USE IN FSHLD
      ENDIF
      IF USED('FSPRD')
        USE IN FSPRD
      ENDIF
      IF USED('ICISTRU')
        USE IN ICISTRU
      ENDIF
      IF USED('GLSETUP')
        USE IN GLSETUP
      ENDIF
      IF USED('APSETUP')
        USE IN APSETUP
      ENDIF
      IF USED('GLACCHAR')
        USE IN GLACCHAR
      ENDIF
      DECLARE laInstModl[1]
      =gfSubStr(laData[15],@laInstModl,'|')
      lnI = 1
      FOR lnI = 1 TO ALEN(laInstModl,1)
        =lfInstall(laInstModl[lnI])
      ENDFOR
      *-- copy setups file.
      lcActSetup = gfTempName()
      lcHstSetup = gfTempName()
      =gfOpenFile(lcSorDir+'setups','modvar','SH',@lcActSetup,.T.)
      =gfOpenFile(lcTrgDir+'setups','modvar','SH',@lcHstSetup,.T.)
      SELECT (lcActSetup)
      LOCATE
      SCAN
        SCATTER MEMO MEMVAR
        IF !SEEK(EVALUATE(KEY()),lcHstSetup)
          SELECT (lcHstSetup)
          APPEND BLANK
        ENDIF
        SELECT (lcHstSetup)
        GATHER MEMO MEMVAR
        SELECT (lcActSetup)
      ENDSCAN
      IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcActSetup)
        SELECT (lcActSetup)
        REPLACE MDATA_DEF WITH laData[1]
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcActSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH laData[1]
        ENDIF
      ENDIF
      =gfCloseFile(lcActSetup)
      IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID',lcHstSetup)
        SELECT (lcHstSetup)
        REPLACE MDATA_DEF WITH M_COMP_ID
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'2','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcHstSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH M_COMP_ID
        ENDIF
      ENDIF
      IF SEEK (CHR(255)+CHR(255)+'LLHIST',lcHstSetup)
        SELECT (lcHstSetup)
        REPLACE MDATA_DEF WITH IIF(llHist,'.T.','.F.')
      ELSE
        IF SEEK (CHR(255)+CHR(255)+SPACE(9)+'1','SYCCONFG')
          SELECT SYCCONFG
          SCATTER MEMO MEMVAR
          SELECT (lcHstSetup)
          APPEND BLANK
          GATHER MEMO MEMVAR
          REPLACE MDATA_DEF WITH IIF(llHist,'.T.','.F.')
        ENDIF
      ENDIF
      =gfCloseFile(lcHstSetup)
      =gfOpenFile(lcTrgDir+'setups','modvar','SH')
      *-- copy ICISTRU file.
      IF FILE(lcSorDir+'icistru.dbf')
        COPY FILE lcSorDir+'icistru.cdx' TO lcTrgDir+'icistru.cdx'
        COPY FILE lcSorDir+'icistru.dbf' TO lcTrgDir+'icistru.dbf'
      ENDIF
      *-- copy APSETUP file.
      IF FILE(lcSorDir+'apsetup.dbf')
        COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.dbf'
      ENDIF
      *-- copy GLSETUP file.
      IF FILE(lcSorDir+'glsetup.dbf')
        COPY FILE lcSorDir+'glsetup.dbf' TO lcTrgDir+'glsetup.dbf'
        COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.fpt'
      ENDIF
      *-- copy GLACCHAR file.
      IF FILE(lcSorDir+'glacchar.dbf')
        COPY FILE lcSorDir+'glacchar.cdx' TO lcTrgDir+'glacchar.cdx'
        COPY FILE lcSorDir+'glacchar.dbf' TO lcTrgDir+'glacchar.dbf'
      ENDIF
      *-- copy account code file.
      COPY FILE lcSorDir+'accod.cdx' TO lcTrgDir+'accod.cdx'
      COPY FILE lcSorDir+'accod.dbf' TO lcTrgDir+'accod.dbf'
      *-- copy sequence file.
      COPY FILE lcSorDir+'sequence.cdx' TO lcTrgDir+'sequence.cdx'
      COPY FILE lcSorDir+'sequence.dbf' TO lcTrgDir+'sequence.dbf'
      *-- copy fiscal year files.
      COPY FILE lcSorDir+'fishd.cdx' TO lcTrgDir+'fishd.cdx'
      COPY FILE lcSorDir+'fishd.dbf' TO lcTrgDir+'fishd.dbf'
      COPY FILE lcSorDir+'fishd.fpt' TO lcTrgDir+'fishd.fpt'
      COPY FILE lcSorDir+'fshld.cdx' TO lcTrgDir+'fshld.cdx'
      COPY FILE lcSorDir+'fshld.dbf' TO lcTrgDir+'fshld.dbf'
      COPY FILE lcSorDir+'fsprd.cdx' TO lcTrgDir+'fsprd.cdx'
      COPY FILE lcSorDir+'fsprd.dbf' TO lcTrgDir+'fsprd.dbf'
      lnI = 1
      FOR lnI = 1 TO ALEN(laOGObjTyp,1)
        =lfOGShowGet(laOGObjTyp[lnI,1],.F.)
      ENDFOR
      SHOW GET pbOGReset DISABLE
      SHOW GET pbCancel DISABLE
      *--- swich to view mode for not hanging when use scroll bal
      laScrMode    = .F.
      laScrMode[2] = .T.
    ELSE
      IF !GfGetMemVar('LLHIST',M_Comp_Id)
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This Company has a History Company already.")
      ELSE
        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This is a History Company.")
      ENDIF
      _CUROBJ = _CUROBJ
    ENDIF
  ELSE
    =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"This company does not exist.")
    _CUROBJ = _CUROBJ
  ENDIF
  SELECT (lnAlias)
ELSE
  LLHIST = .F.
  *B604095,1 AMH 01/08/2001 Enable all Setups before lfOGRShow [Start]
  lnI = 1
  FOR lnI = 1 TO ALEN(laOGObjTyp,1)
    =lfOGShowGet(laOGObjTyp[lnI,1],.T.)
  ENDFOR
  *B604095,1 AMH [End]
  =lfOGRShow()
ENDIF
*--- end of lfvHistCmp.

*!*************************************************************
*! Name      : lfvllHist
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/23/2000
*! Purpose   : Valid function of history company.
*! Reference : *E301469,1 AMH
*!*************************************************************
*! Called from : SMCMSET.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvllHist()
*!*************************************************************

FUNCTION lfvllHist
IF gcAct_Comp == laData[1]
  =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
              "You cannot select the current company to be a History Company.")
  LLHIST = .F.
  =lfOGShowGet(laOGObjTyp[ASUBSCRIPT(laOGObjTyp,ASCAN(laOGObjTyp,'LLHIST'),1),1],.T.)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnI = 1
FOR lnI = 1 TO ALEN(laOGObjTyp,1)
  =lfOGShowGet(laOGObjTyp[lnI,1],.F.)
ENDFOR
=lfOGShowGet(laOGObjTyp[ASUBSCRIPT(laOGObjTyp,ASCAN(laOGObjTyp,'M_COMP_ID'),1),1],.T.)
*--- end of lfvllHist.

*!**************************************************************************
*! Function    : lfvUnqStPr
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 12/03/2000
*! Purpose     : Unique Site Prefix Validation
*!**************************************************************************
*E301488,1
FUNCTION lfvUnqStPr
PRIVATE lcCurrObj, lcCurrVal , lnActRecord , lcErrMsg

lcCurrObj = SYS(18)
lcCurrVal = ALLTRIM(EVALUATE(lcCurrObj))
lcErrMsg = ""
*B604337,1 AMM start 29/03/2001 remove the condition , it could be empty.
*IF EMPTY(lcCurrVal)
*  lcErrMsg = "You cannot left the unique site prefix empty."
*ELSE
IF !EMPTY(lcCurrVal)
*B604337,1 AMM end
  SELECT SYCCOMP
  lnActRecord =  RECNO()
  SCAN FOR !(CCOMP_ID == laData[1])
    IF gfGetMemVar("M_UNQSTPRX",CCOMP_ID) = lcCurrVal
      lcErrMsg = "Company " + CCOMP_ID +  " has the same unique site prefix."     
      EXIT
    ENDIF  
  ENDSCAN
  *B804062,1 NAD  04/09/2002 (START) 
  IF BETWEEN(lnActRecord,1,RECCOUNT('SYCCOMP'))
    GOTO  lnActRecord IN SYCCOMP
  ENDIF
  *B804062,1 NAD  04/09/2002 (END)
ENDIF

IF !EMPTY(lcErrMsg)
  =gfModalGen("TRM000000B00000","DIALOG",'','',lcErrMsg)
  &lcCurrObj = ""
  _CUROBJ = _CUROBJ
ENDIF
*-- end of lfvUnqStPr.


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
lnPos =  ASCAN(laRelFld,'CCHKACCT',1)
IF lnPos = 0
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
  IF !SEEK(EVAL(lcVar),'APBANKS') OR cBnkType <> 'S'
    lcBrFields = "cBnkCode :H='Bank code',cBnklndes :H='Long description' "
    =gfBrows("FOR cBnkType = 'S' ",'cBnkCode','laTemp')
    IF !EMPTY(laTemp[1])
      &lcVar = laTemp[1]
    ELSE 
      &lcVar = SPACE(8)
    ENDIF
  ENDIF
ELSE
  lnPos = ASUBSCRIPT(laRelFld,lnPos,1 )
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
  IF !SEEK(EVAL(lcVar),'APBANKS') OR cBnkType <> 'B'
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
ENDIF
IF !EMPTY(lcOldBnk)
  USE IN APBANKS
  =gfOpenFile(lcOldBnk,'','SH')
ENDIF

*!**************************************************************************
*! Function    : lfvSiteTyp
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 11/30/2000
*! Purpose     : Validation for site type (No more than one Back Office).
*!**************************************************************************
*E301488,1
FUNCTION lfvSiteTyp
PARAMETERS lcVar , lcReturn , lcComp , lcOldRltd 
PRIVATE lcVar , lcReturn , lcComp , lcOldRltd, lnPopValue
lnPopValue   =  EVALUATE(SYS(18)) 
*-- if Back office was selected
IF "NC" $ gcCmpModules .AND. lnPopValue = 2 AND USED(lc_TmpFl)
  SELECT cCode_No FROM (lc_TmpFl) ;
    WHERE cdefcode+crltfield+cfld_name = "NYCSITEID" AND;
          crltd_nam = "CCMSITETYP" AND LEFT(crltd_vlu,1) = "B";
          INTO ARRAY laSiteID      

  *-- if there is another back office site
  IF _TALLY > 0
    =gfModalGen("TRM000000B00000","DIALOG",'','',"Site " + ALLTRIM(laSiteID[1]) +;
             " is the back office, cannot assign more than one back office.")
    &lcReturn = .F.
  ENDIF    
ENDIF
*-- end of lfvSiteTyp.



*!**************************************************************************
*! Function    : lfvCompLog
*! Author      : Saeed Mohammed Mostafa(SMM)
*! Date        : 07/15/2004
*! Purpose     : Validation for Logo Path (BMP only)
*! Entry       : E038226	
*!**************************************************************************
FUNCTION lfvCompLog
lcPath = EVALUATE(SYS(18)) 
IF !EMPTY(lcPath)
  IF !(UPPER(Right(AllTrim(lcPath),3))='BMP')
    =gfModalGen("TRM00000B00000","DIALOG","Company Information",.F.,;
                "The logo must be a Bitmap(BMP) ")
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF

