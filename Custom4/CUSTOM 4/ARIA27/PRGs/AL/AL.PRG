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
*B804306,1 HBG 04/03/2002 Save field of To Store or consalidatore in BOL_HDR.
*B804525,1 ADEL 09/25/02 Avoid repeating BOL in BOL_HDR in any cases.
*B606788,1 Reham 12/18/2002 Do not include the "******" piktkt in the piktkt list.
*E302192,1 WAB 08/14/2003 Change a default on Bill Of Lading Screen , according to Partner Setup
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*****************************************************************************

PARAMETERS lcProgName,lcPrgPram

EXTERNAL PROCEDURE GFOPGRID
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

*E301164,1 Hesham (end)
  *Renee
  DECLARE   laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]
  STORE '' TO laOGFxFlt,laOGVrFlt, lcReportID   ,laRepFltr
  *Renee end
  
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

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N        ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl       ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields       ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp          ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr       ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd       ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce ,;
               lcSydKey     , lcNoteType      , lcNoteKey

  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N        ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl       ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields       ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp          ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr       ,;
               laWobjects   , lcButtNam       , lcCurr_yer      , lcCurr_prd       ,;
               laSubProc    , lcExDifAcc      , lcCompPrnt,laPrgTemps,lcMenUnProce ,;
               lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

*C101459,1 Hesham (End)


*-- HDM E301176,1 03/21/1999 [End]

*E301164,1 Hesham (end)
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
  *-- HDM E301176,1 03/21/1999 [Start]
  laCtrStat[13] = "DISABLE"
  *-- HDM E301176,1 03/21/1999 [End]
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
llPrgFound =(GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
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
  llPrgFound =(GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
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
*! Name      : lfGetPkTkt                      
*! Developer : Reham Al-Allamy
*! Date      : 02/25/97
*! Purpose   : Get a picking ticket for the sended order no.
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lcOrdNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfGetPkTkt(lcOrdNo , OrdHdr.Division)
*!*************************************************************
*! Modifications     :
*E300511,1 Reham On 07/23/97
*E300511,1 Fix the seek to get the right picking ticket.
*!*************************************************************
*E300989,1 MAB 09/27/1998 Skip ask user for generate new PickTkt
*E300989,1                if new passed parameter is either (Y or N)
*
FUNCTION lfGetPkTkt

*E300676,1 Change this line to add 2 new parameters for the store and the
*warecode [Begin]
*PARAMETERS lcOrdNo , lcDivision
*PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode
*E300676,1 Change this line to add 2 new parameters [End]

*E300989,1 Change the following line adding new parameter for skip ask [begin]
*E300989,1 user for generate new pick ticket.
*PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode
PARAMETERS lcOrdNo , lcDivision , lcStore , lcWareCode , lnAskType
*E300989,1 Change the following line adding new parameter for skip ask [end]

*E300989,1 Assure that it's defaulted to ask avoiding problems by unpassed parameter.
IF TYPE('lnAskType') $ 'UL' 
  lnAskType = 0
ENDIF
*E300989,1 Assure that [end]

PRIVATE lcOrdNo   , lcDivision , lcCurAlias , lcPikTkt , ;
        llOpnPkTk , lnOldTag   , lnRecPkTk  , laPikNo  , lcExact , llAskUser

llAskUser = (lnAskType = 0)
*-- Save the exact setting.
lcExact = SET("EXACT")

*-- Define array hold the piktkts no. , status , printed or not.
DECLARE laPikNo[1]

*-- Save the current alias.
lnCurAlias = SELECT()

*-- Initialize the needed variables.
STORE ""  TO laPikNo , lcPikTkt
STORE .F. TO llOpnPkTk
STORE 0   TO lnOldTag   , lnRecPkTk

*-- Open the needed files.
*IF !USED('PikTkt')
  *-- Open the file if it was not open. 
*  =gfOpenFile(QDD+'PikTkt',QDD+'OrdPik','SH')
*  llOpnPkTk = .T.
*ELSE
SELECT PikTkt
lnOldTag = INT(VAL(SYS(21)))
SET ORDER TO TAG OrdPik
*-- Save the current record no. if the file was open.
lnRecPkTk = RECNO("PikTkt")
*ENDIF

SELECT PikTkt
*E300511,1 Reham On 07/23/97  *** Begin ***
*IF SEEK(lcOrdNo , "PikTkt")
*RENEE 
*IF SEEK(lcOrdNo , "PikTkt") .AND. ;
        !(PikTkt.Status $ "CX") .AND. ;
        Store = lcStore .AND. cWareCode = lcWareCode
IF SEEK(lcOrdNo , "PikTkt") 
  *B606788,1 Reham On 12/18/2002  *** Begin ***
  *B606788,1 Do not include the "******" piktkt in the piktkt list
  *LOCATE REST WHILE Order = lcOrdNo;
  *       FOR !(PikTkt.Status $ "CX") .AND. ;
  *       Store = lcStore .AND. cWareCode = lcWareCode        
  LOCATE REST WHILE Order = lcOrdNo;
         FOR !(PikTkt.Status $ "CX") .AND. ;
         Store = lcStore .AND. cWareCode = lcWareCode AND !("*" $ piktkt)
  *B606788,1 Reham On 12/18/2002  *** End   ***
  IF FOUND()       
*RENEE end   
*E300511,1 Reham On 07/23/97  *** End   ***

    *-- If there was any piktkts created for any of the 
    *-- current order lines.
    *E300511,1 Reham On 07/23/97  *** Begin ***
    *E300511,1 Comment the if condition to add it to the seek condition.
    *E300676,1 Add this IF statment to check the Store and the Warecode [Begin]
    *IF Store = lcStore .AND. cWareCode = lcWareCode
    *E300676,1 Add this IF statment to check the Store and the Warecode [End]
    *E300511,1 Reham On 07/23/97  *** END   ***
    laPikNo[ALEN(laPikNo,1)] = Piktkt.piktkt + " " + ;
              IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
              IIF(Piktkt.status $ "O ","Open   ",IIF(Piktkt.status="H" , "On hold" , SPACE(7))) + ;
              " " + DTOC(PikTkt.Date)
  
    *E300511,1 Reham On 07/23/97  *** Begin ***
    *E300676,1 Add ENDIF for the IF statment to check the Store and the 
    *Warecode [Begin]
    *ENDIF    && End of IF
    *E300676,1 Add ENDIF for the IF statment [End]
    *E300511,1 Reham On 07/23/97  *** Begin ***
  
    SKIP
    IF EOF()
      SKIP -1
    ENDIF
  
    *E300676,1 Change this SCAN loop to check the Store and the Warecode [Begin]
    *SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo FOR !(PikTkt.Status $ "CX")
    *B606788,1 Reham On 12/18/2002  *** Begin ***
    *B606788,1 Do not include the "******" piktkt in the piktkt list
    *SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo ;
    *            FOR !(PikTkt.Status $ "CX") .AND. Store = lcStore .AND. cWareCode = lcWareCode
    SCAN REST WHILE PikTkt.Order + PikTkt.PikTkt = lcOrdNo ;
                FOR !(PikTkt.Status $ "CX") .AND. Store = lcStore .AND. cWareCode = lcWareCode AND !("*" $ piktkt)
    *B606788,1 Reham On 12/18/2002  *** End   ***
   
    *E300676,1 Change this SCAN loop to check the Store and the Warecode [End]
    
      *-- Set excat to off.
      SET EXACT OFF
      IF ASCAN(laPikNo , PikTkt.PikTkt) = 0
      
        *E300676,1 Add this IF statment to check if the last element is not
        *empty
        IF !EMPTY(laPikNo[ALEN(laPikNo,1)])
        *E300676,1 Add this IF statment [End]
        
          DIMENSION laPikNo[ALEN(laPikNo,1)+1]
        
        *E300676,1 Add ENDIF for the IF statment to check if the last element
        *is not empty
        ENDIF    && End of IF
        *E300676,1 Add ENDIF for the IF statment [End]

        laPikNo[ALEN(laPikNo,1)] = Piktkt.piktkt + " " + ;
                IIF(Piktkt.prtflag="P","Yes","No ") + SPACE(5) + ;
                IIF(Piktkt.status $ "O ","Open   ",IIF(Piktkt.status="H" , "On hold" , SPACE(7))) + ;
                " " + DTOC(PikTkt.Date)
      ENDIF
      *-- Restore the excat setting.
      SET EXACT &lcExact
    ENDSCAN
    IF ALEN(laPikNo,1) > 1
      lsPikLst = 1
      
      *E300989,1 if user want to generate new from passed parameter [begin]
      *E300989,1 we does not ask him and always generate new.
      *DO ALPIKLST.SPR
      IF llAskUser
        DO (gcScrDir + gcWinAppl + '\ALPIKLST.SPX')
      ELSE && Add new pick tickt.
        =lfvGetPik(2)
      ENDIF
      *E300989,1 if user want to generate new from passed parameter [end]
      
    ELSE
      *Reham On 17/12/97  ** Begin **
      *IF SUBSTR(laPikNo[1],8,1) = "P"
      
      *E300676,1 Change this line to change gfDialog() to gfModalGen() [Begin]
      *IF gfDialog("?" , "There is a printed picking ticket","\<Add;\<New") = 1
      *** There is Picking ticket no. : {laPikNo[1]}.     ***
      *** Do you wish to assign a new picking ticket no.? ***
      *** < Add > - < New > ***
      *IF gfModalGen("TRM44011B44000","DIALOG") = 1

      *E300989,1 Add the following lines to skip ask  [begin]
      *E300989,1 if passed asktype is not zero, and change gfmod to be in variable.
      
      lnAskType = IIF(lnAskType=0,gfModalGen("TRM44011B44000","DIALOG" , SUBSTR(laPikNo[1],1,6)),lnAskType)
      
      *E300989,1 Change the following line to If lnAskType instead of gfModalGen.
      *IF gfModalGen("TRM44011B44000","DIALOG" , SUBSTR(laPikNo[1],1,6)) = 1
      IF lnAskType = 1
      *E300676,1 Change this line to change gfDialog() to gfModalGen() [End]
      *E300989,1 Add the following lines to skip ask  [end]
      
        lcPikTkT = SUBSTR(laPikNo[1],1,6)
      ELSE  
        *lcPikTkT = Sequence('PikTkt',lcDivision)
        lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
      ENDIF
      *ELSE
      *  lcPikTkT = SUBSTR(laPikNo[1],1,6)
      *ENDIF
      *Reham On 17/12/97  ** End   **
    ENDIF
  *RENEE endif
  ELSE
    *-- If the order no. not found in the PikTkt file, create new piktkt.
    *lcPikTkT = Sequence('PikTkt' , lcDivision)
    lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
  ENDIF
  *RENEE endif end
ELSE
  *-- If the order no. not found in the PikTkt file, create new piktkt.
  *lcPikTkT = Sequence('PikTkt' , lcDivision)
  lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
ENDIF

*IF llOpnPkTk
  *-- If the file was opened in this session, close it.
*  USE IN PikTkt
*ELSE
SELECT PikTkt
SET ORDER TO lnOldTag
*-- File was opened in previous session, Set the opinter to th right record.
IF lnRecPkTk > 0 .AND. lnRecPkTk <= RECCOUNT("PikTkt")
  GOTO lnRecPkTk
ENDIF
*ENDIF

*-- Restore the exact setting.
SET EXACT &lcExact

*-- Restore the old alias.
SELECT (lnCurAlias)

*-- Return with the selected picking ticket no.
RETURN lcPikTkt
*-- end of lfGetPkTkt. GFMOD


*!*************************************************************
*! Name      : lfvGetPik
*! Developer : Reham Al-Allamy
*! Date      : 02/25/97
*! Purpose   : Valid function for both buttons : Select - New
*!*************************************************************
*! Calls      : None
*!*************************************************************
*! Passed parameters : lnButtonNo
*!*************************************************************
*! Returns           :  None
*!*************************************************************
*! Example           :  =lfvGetPik(lnButtonNo)
*!*************************************************************
*
FUNCTION lfvGetPik
PARAMETERS lnButtonNo

*-- If lnButtonNo = 1, Select an existing picking ticket from the list.
*-- If lnButtonNo = 2, Get new Picking ticket.

DO CASE
  CASE lnButtonNo = 1
    lcPikTkT = SUBSTR(laPikNo[lsPikLst],1,6)
  CASE lnButtonNo = 2
    *-- Get new picking ticket.
    *lcPikTkT = Sequence('PikTkt' , lcDivision)  
    lcPikTkT = gfSequence('PIKTKT', '', '', lcDivision)
ENDCASE


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
*--

*!*************************************************************
*! Name      : lfF3Key
*! Developer : RAMY MABROUK
*! Date      : 06/28/2000
*! Purpose   : Trap the F3 key
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Example   :  =lfF3Key()
*!*************************************************************
*
*E301434,1

FUNCTION lfF3Key

PRIVATE lcCurrObj
lcCurrObj = SYS(18)

DO CASE 
  CASE TYPE(lcCurrObj) = 'C'
    STORE SPACE(0) TO &lcCurrObj

  CASE TYPE(lcCurrObj) = 'D'
    STORE {} TO &lcCurrObj
    
ENDCASE

*!*************************************************************
*! Name      : lfGetBOL
*! Developer : HEND GHANEM (HBG)
*! Date      : 06/12/2000
*! Purpose   : Check IF there is an open BOL use it other wise generate new one
*!*************************************************************
*! Parameters: lcAcc      : The account for this packing list
*!           : lcStore    : The Store for this packing list
*!           : lcLoc      : The WareHouse for this packing list
*!           : lcShpVia   : The ShipVia for this packing list
*!           : lcStnCartn : The defult value of the field "StanCarton" for the BOL_HDR
*!                       :* Previuos parameters used in the validation expression to get the
*!                       :* the BOL#.
*!           : lcFldArray : parameter hold the name of the array which hold defulted 
*!                        : values of the fields of BOL_HDR.
*!           : llGenerate : This parameter passed by .T. if want to force 
*!                        : the generation of BOL. When it passed by .T. , the 
*!                        : parameter "llAskMe" should be .F.
*!           : llAskMe    : This parameter is .T. if want to ask user about 
*!                        : the generation of BOL. When it passed by .T. , the 
*!                        : parameter "llGenerate" should be .F.
*!*************************************************************
*! Returns   : BOL #
*!*************************************************************
*! Example   : lfGetBOL()
*!*************************************************************
*C101891,1
*B803177,1  
*!
FUNCTION lfGetBOL
*B804306,1 HBG 04/03/2002 Add parameter of To Store or consalidatore[Begin]
*PARAMETERS lcBollsttl , lcAcc, lcStore, lcLoc,lcShpVia , lcStnCartn , lcFldArray , llGenerate , lcBolVal
PARAMETERS lcBollsttl , lcAcc, lcStore, lcLoc,lcShpVia , lcStnCartn , lcFldArray ,;
            llGenerate , lcBolVal , lcToStorCn
*B804306,1 [End]

PRIVATE lcAcc, lcStore, lcLoc,lcShpVia,lcPack_No , lnCurAlias , lcActivBOL ,lcStnCartn  ,;
        laBolFilds, llGenerate , llAskMe ,lvMemeVar  , lcBolStuts ,lcMessage;
        llForceGen , lcBolVal ,lcshpViaDs, lcBollsttl


llAskMe = .F.
IF TYPE("lcBolVal") $ "UL"
  lcBolVal = ""
ENDIF
*B804306,1 HBG 04/03/2002 IF parameter of To Store or consalidatore not 
*B804306,1                   passed set it to store as defult [Begin]
IF TYPE("lcToStorCn") $ "UL"
  lcToStorCn = 'S'
ENDIF
*B804306,1 [End]

llForceGen = llGenerate  && Variable to determan if force generation or not

*-- If Account and location is empty ,retern
IF EMPTY(lcAcc) OR EMPTY(lcLoc)
  RETURN ""
ENDIF && If Account and location is empty ,retern

*-- Check if  AS modules are installed or not , if not so we won't 
*-- generate BOL#
IF ('AS' $ gcCmpModules)
  lnCurAlias = SELECT(0)
  *-- IF programmer does not force the generation of BOL. 
  *-- Go Normal BOL algorithm according to the setup 
  IF !llGenerate  
  
    lcBolStuts = gfGetMemVar('M_BOLSTUTS',gcAct_Comp)
    *-- If the setting of BOL setup is never so return
    IF lcBolStuts = "N"
      SELECT (lnCurAlias)
      RETURN ""
    ENDIF  && end If the setting of BOL setup is never
      
    *-- Check setting of BOL.
    DO CASE
     *-- If the setting of BOL setup is Inquire
     CASE lcBolStuts = "I"
       llGenerate = .F.
       llAskMe    = .T.
     *-- If the setting of BOL setup is Automatic
     CASE lcBolStuts = "A"
       llGenerate = .T.
       llAskMe    = .F.
    ENDCASE 

  ENDIF && If we don't generate BOl by store ,check the BOL setting 

  lcActivBOL = ""
  =gfOpenFile(gcDataDir+'Bol_Hdr',gcDataDir+'Bol_Hdr','SH')

  lcDistCtr  = lcStore
  *-- Get the value of Distrepution center From Customer file
  IF SEEK(IIF(EMPTY(lcStore),'M','S')+lcAcc+lcStore,'Customer')
    lcDistCtr  = IIF(EMPTY(Customer.Dist_Ctr),Customer.Store,Customer.Dist_Ctr)
  ENDIF

  lnCount = 1
  DIMENSION laBolInf[1]     && array to hold informations of BOL if it found
  laBolInf = ""
  
  SELECT BOL_HDR
  *-- Scan to get open BOL for this account,Dist. center,store,and shipvia and fill the array 
  *-- with the open BOL no. and date
  SCAN FOR (Account+Store+W_Code+ShipVia+StanCarton =lcAcc+lcDistCtr+lcLoc+lcShpVia+lcStnCartn);
       AND (Status <> "C")
    IF !llForceGen
      DIMENSION laBolInf[lnCount]
      laBolInf[lnCount] = BOL_NO + SPACE(12) + DTOC(BolDate)
    ENDIF

    *-- If the setting og BOL setup is Automatic or user didn't enter a value for BOL exit scan
    IF llGenerate AND EMPTY(lcBolVal)
      EXIT
    ENDIF
    
    *-- IF the value entered by user is valid save only it in the array.
    IF (lcBolVal == BOL_NO)
      STORE .F. TO llGenerate , llAskMe
      DIMENSION laBolInf[1]
      laBolInf[1] = BOL_NO + SPACE(12) + DTOC(BolDate)
      EXIT
    ENDIF
    
    lnCount = lnCount + 1
  
  ENDSCAN && end scan to get BOL for this account,Dist. center,store,and shipvia.     
  
  *--Get ShipVia Description
  lcshpViaDs = gfCodDes(lcShpVia,"ShipVia")
  
  *-- Check if there is BOL found or not, and if it found is it one or more than one.
  
  DO CASE
    *-- IF there is no open BOL
    CASE EMPTY(laBolInf[1])
      lcActivBOL = ""
      *-- If the setting of BOL setup is Inquire 
      IF llAskMe
        lcMessage = "Account : " + lcAcc +;
                    IIF(EMPTY(lcDistCtr),"",", Dist. Center : " +ALLTRIM(lcDistCtr)) +;
                    ", Location : "+ ALLTRIM(lcLoc) +;
                    ", and Ship Via : " + ALLTRIM(lcshpViaDs)
                    
        *-- Message : No BOL found for Account : X, Dist. Center : Y, Location : Z 
        *--           and ShipVia S.
        *--           Do yo want to create a new BOL?"   
        *-- <Yes><No>
        llGenerate = (gfModalGen("INM44100B38006","Dialog",lcMessage)=1)

      ENDIF
    
    *-- IF there is only one open BOL
    CASE ALEN(laBolInf) = 1
      *-- If the setting of BOL setup is Automatic ,check if there is an open BOL or not
      llGenerate = llGenerate AND EMPTY(laBolInf[1])
      *-- take this open BOL
      lcActivBOL = SUBSTR(laBolInf[1],1,6)

      *-- If the setting of BOL setup is Inquire 
      
      IF llAskMe
        lcMessage = "Account : " + lcAcc +;
                    IIF(EMPTY(lcDistCtr),"",", Dist. Center : " +ALLTRIM(lcDistCtr)) +;
                    ", Location : "+ ALLTRIM(lcLoc) +;
                    ", and Ship Via : " + ALLTRIM(lcshpViaDs)

        *-- 'BOL#: X found for Account: Y ,Dist. Center : Z ,Location : K ,and Ship Via : J'
        *-- <Assign><Create new><Cancel>
        lnChoice = gfModalGen("INM44101B44012","Dialog", lcActivBOL+"|"+lcMessage)
        llGenerate = (lnChoice = 2)
        *-- If user select "Cancel"
        IF lnChoice = 3 
          lcActivBOL = ""
        ENDIF
      ENDIF
      
    *-- IF there is more than one open BOL
    CASE ALEN(laBolInf) >1
      
      *-- If the setting of BOL setup is Automatic ,check if there is an open BOL or not
      llGenerate = llGenerate AND EMPTY(laBolInf[1])
      *-- take this open BOL
      lcActivBOL = SUBSTR(laBolInf[1],1,6)
      
      *-- If the setting of BOL setup is Inquire 
      IF llAskMe
        *-- Call Select BOL Screen
        
        lcBollsttl = "Available B.O.L list " + lcBollsttl

        DO (gcScrDir + gcWinAppl + '\ALBOLLST.SPX')
      ENDIF
   
  ENDCASE && End check if there is BOL found or not


  *-- If we go to Generate BOL because user want that or force generation
  IF llGenerate
    *-- If the array that hold the initial value of BOL_HDR fields is empty , Intialize it
    IF (TYPE(lcFldArray) $ "UL") OR (TYPE(lcFldArray+"[1,1]") $ "UL") OR EMPTY(&lcFldArray[1,1])
      DIMENSION &lcFldArray[3,2]
      &lcFldArray[1,1] = "cgronhang"
      &lcFldArray[1,2] = "N"
      &lcFldArray[2,1] = "ctranmthd"
      &lcFldArray[2,2] = "M"
      &lcFldArray[3,1] = "packtype"
      &lcFldArray[3,2] = "CTN25"  
    ENDIF && If the array that hold the initial value of BOL_HDR fields is empty , Intialize it
 
    *-- Generate new BOL 
    lcActivBOL = gfSequence('BOL_NO')

    *-- Get the value of carreier code which is related field to Shipvia code
    DECLARE laVRltFld[1,2]
    laVRltFld[1,1] = 'CARRIERCOD'
    laVRltFld[1,2] = 'lcCarCod'
    lcCarCod = ''
    =gfRltFld(lcShpVia ,@laVRltFld,'SHIPVIA   ')
    
    *-- Insert record to the new BOL in BOL_HDR file.  
    m.BOL_NO     = lcActivBOL
    m.ACCOUNT    = lcAcc
    m.STORE      = lcDistCtr
    m.W_CODE     = lcLoc  
    m.ShipVia    = lcShpVia
    m.Carrier    = gfCodDes(m.ShipVia,'SHIPVIA')
    m.CarrierCod = lcCarCod
    m.StanCarton = lcStnCartn
    *B804306,1 HBG 04/03/2002 Save field of To Store or consalidatore in BOL_HDR [Begin]
    m.cToStorCn  = lcToStorCn
    *B804306,1 [End]


    *-- assign the defult values of BOL which holded in the passed array to memory var.
    FOR lnI = 1 TO ALEN(&lcFldArray,1)
     lvMemeVar  = "m." + &lcFldArray[lnI,1]
     &lvMemeVar = &lcFldArray[lnI,2]
    ENDFOR
    m.BolDate    = gdSysDate
    m.cBolTime   = Time()
    *E302192,1 WAB (Start) Change a default on Bill Of Lading Screen , according to Partner Setup
    IF SEEK('A'+m.ACCOUNT,'EDIACPRT','ACCFACT') AND SEEK(EDIACPRT.cPartCode,'EDIPH')
      m.cgronhang = IIF(EDIPH.lGrmHang,'Y','N')
    ENDIF
    *E302192,1 WAB (End) 
    *B804525,1 (Begin) Avoid repeating BOL in BOL_HDR in any cases.
    DO WHILE SEEK(m.BOL_NO,'BOL_HDR')
      m.BOL_NO = gfSequence('BOL_NO')
    ENDDO
    *B804525,1 (End)
    
    INSERT INTO BOL_HDR FROM MEMVAR
  ENDIF && End If Generate BOL
    
 
  SELECT(lnCurAlias)

ENDIF
RETURN lcActivBOL
*-- End Of lfGetBOL

*!*************************************************************
*! Name      : lfvGetBol
*! Developer : HEND GHANEM (HBG)
*! Date      : 06/27/2000
*! Purpose   : Check Which Push button is pressed in the select screen 
*!           : of BOL
*!*************************************************************
*! Returns   : BOL # or Flag to tell Generate Bol or not
*!*************************************************************
*! Example   : lfvGetBol()
*!*************************************************************
*C101891,1  
*B803177,1
*!
FUNCTION lfvGetBol
PARAMETERS lnPbutton
PRIVATE lnPbutton

DO CASE
  *-- If user Select BOL 
  CASE lnPbutton = 1
    lcActivBOL = SUBSTR(laBolInf[lnBolNo],1,6)
  *-- If user want to create new BOL 
  CASE lnPbutton = 2
    llGenerate = .T.
  *-- If user select "Cancel"  
  CASE lnPbutton = 3
    lcActivBOL = ""
ENDCASE
*-- end of lfvGetBol
