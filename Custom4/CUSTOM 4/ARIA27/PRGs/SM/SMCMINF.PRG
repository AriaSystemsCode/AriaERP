*:************************************************************************
*:
*: Procedure file: SMCMINF.PRG 
*:                 (Company Information)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300293,1 Hesham 09/30/95 Adding default currency code for the company information
*B800241,1 Hesham 10/15/95 Define bar with prompt "No companies available" in case
*B800241,1                 we delete the only company defined at first time the prog. runs.
*B600799,1                 Change the LOOKUP(SYCINT.CPARt2LAB,IIF(EMPTY(laData[16]),'USA',laData[16]),sycint.ccont_code,'CCONTCODE')
*B600799,1                 TO be LOOKUP(SYCINT.CPARt2LAB,IIF(EMPTY(laData[16]),gcIntCont,laData[16]),sycint.ccont_code,'CCONTCODE') in the screen.
*B600906,1 MAN    03/28/96 Do not display a message if the P.Comp field has ? 
*B601281,1 M.H    09/26/96 Update the global variable called 'gcContCode' with the country code if the company selected is the active company.
*B601309,1 Reham  12/04/96 Check if the dbfs directory exist or not 
*B601309,1                 if it was fresh intallation.
*B601484,1 M.H    12/15/96 Default the country with the default country of the system.
*B601531,1 M.H    01/08/97 Enable the address in the edit mode under windows 
*B601515,1 M.H    01/14/97 Fix the message.
*E300627,1 Hesham El-Sheltawi  03/31/97
*E300627,1 add the module that was installed for the comp. installed field
*E300627,1 modules & remove the modules that was uninstalled
*B601701,1 Hesham El-Sheltawi 04/08/97
*B601701,1 colect the indexes and the structure of the file name 
*B601701,1 padded right with 8 char so it wont
*B601701,1 get wrong structure in case of the file name
*B601701,1 length is less that 8 char
*E300633,1 Hesham El-Sheltawi 04/09/97
*E300633,1 Make the system run the global module setup program if the
*E300633,1 selected module to be installed do not have it's own setup
*E300633,1 program
*E300633,1 change the validation function for the push button setup in 
*E300633,1 the company information screen "SMCMINF.SCX"
*E300645,1 Hesham El-Sheltawi 04/09/97
*E300645,1 create the system data files for the company
*E300663,1 HS  04/24/97 Make some changes for we have change the file
*E300663,1              SYCSEQUN name to SEQUENCE and make it a data file
*E300663,1              [Change its dir. from SYSFILES to DBFS]
*E300611,1 HS  04/24/97 Make some changes to the function [lfDel_Fil] to
*E300611,1              delete the system files that was removed to the
*E300611,1              data dir.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300643,1 RENEE 06/09/97 - Remove using SYCCODES file
*B601803,1 RENEE 06/24/97 Install module, some files are not created
*B601801,1 Hesham El-Sheltawi 06/25/97
*B601801,1 Make the global variable that hold the installed modules for the 
*B601801,1 company updated if the modified company is the active company
*E300692,5 RENEE 07/02/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,5 					SYCFSPRD, SYCFSHLD
*E30703,1 Hesham 08/13/97
*E30703,1 if intalling ic module then copy file ICISTRU item code struc.
*E30703,1 to the company data dir. with the predefined code struct.
*B601905,1 Hesham El-Sheltawi 11/25/97
*B601905,1 Fix the saving for the memo field that save the installed modules
*B601905,1 for the company
*B602064,1 MAN 08/02/89 Fixed the check if the company is currently used while
*B602064,1 MAN          deleting it
*E300993,1 AMM 09/06/98 Check for the required modules for each module when installing it.
*B602147,1 Hesham 11/12/98
*B602147,1 copy file icistru without checking if it is exist in the
*B602147,1 system files directory so if it does not exit an error
*B602147,1 occus and the module will not be installed
*E301065,1 AMM 11/15/98 Copy or remove the form codes from the DBFS form 
*E301065,1              code tables based on the form codes files in the 
*E301065,1              sysfiles directory.
*B602147,1 Hesham 11/19/1998
*B602147,1 if the user select yes for installing ic module files then
*B602147,1 copy file icistru without checking if it is exist in the
*B602147,1 system files directory so if it does not exit an error
*B602147,1 occus and the module will not be installed
*B602350,1 RENEE 12/14/98. Variable llYes2All not found when creating a 
*B602350,1 				   new company.	
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602658,6 RENEE 03/11/99 1. Fix checking for a fiscal calendar
*B602658,6				  2. Reposition SYCCOMP record pointer on the current company
*B602658,6				     after coming back from executing another program
*E301315,1 Hesham 09/13/99
*E301315,1 If the user is running the setup program for a module that does not
*E301315,1 have an application then run the SM.app insted of the module .app
*B603737,1 Hesham 07/11/2000
*B603737,1 if sort the modules selected to be installed to make the communication module
*B603737,1 first module if selected
*B603744,1 ADEL 07/19/2000 Fix the bug of 'Setups' file doesn't exist in case of no active company selected.
*E301469,1 AMH 11/02/2000 Purge Database.
*B804186,1 NAD 05/06/2001 EDIT OF COMPANY INFO CREATES DUPLICATE RECORDS. 
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*B038431,1 NNA 09/27/2004 Fix bug that ths State field Doesn't have any Validation
*:************************************************************************

EXTERNAL ARRAY laData,laScrMode,laDefproc

DECLARE laModules[1],laCompInfo[2],laAllMod[1],laCountry[1,2]
*DIMENSION laKeyField[1,4],laFileStrc[1,5],laFileCDX[4],laWndObj[3,3]
DIMENSION laKeyField[1,4],laFileStrc[1,5],laFileCDX[4],laWndObj[2,3]

laModules   = ' '
llRemovDir  = .F.
llFlagValid = .F.
llNoComp    = .F.
llOpenBy    = .F.
llSavMod    = .F.
llChngCom   = .F.
llSavFis    = .F.
llSavAcc    = .F.
llError     = .F.
llView      = .F.
llBrowse    = .F.
llNewSetup  = .F.

*E300692,5 Flag to show whether ACCOD is opened from the correct path or not
STORE '' TO lcDataDir
*E300692,5 end

lcInsMdl    = " "
lcOldMod    = " "
lcPrntName  = " "
lcOldPrnt   = " "
lcOldPrnN   = " "
lcOldCmp    = " "
lnComRec    = 0
lnCurRec    = 0

lcDirStat   = " "

lcOldActCm  = ""
lcOldDataD  = ""
lcCountry   = ""

lcDiamond   = ""
lcPrntStat  = ""
lcPrompt    = ""

lcTSelect   = ""  && Hold the word "S\<elect"
lcTUnSelct  = ""  && Hold the word "\<Unselect"
*E300293,1 start definition of extara needed variables

*E300296,1 M.H   10/10/95 Add the currency to the AP module.
lcCurrStat  = ""  && hold the Currancy display Status
lcOldCurr   = '' && hold the old value of the currency
lcMultCurr  = "DISABLE"
lcMultCCnt  = "DISABLE"
*E300296,1 M.H End.

lcSMCmSt    = ""  && hold the Setup window name
lnProgCopy      = gnProgCopy &&	 No of Program copies running at the moment
lcCmStTitl  = "Company Setups "+IIF(lnProgCopy=1,'',' /'+ALLTRIM(STR(lnProgCopy))) && hold the setup window title
lcTempSetup = ""
*E300293,1 end of definition
IF !gfSetup()    
  RETURN
ENDIF  

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'cComp_Id'
laKeyField[1,4] = 1

laDefProc[7]    = .F.
laDefProc[9]    = .F.
laDefProc[10]   = .F.

IF !WEXIST(lcBaseWind)
  *E300293,1 start
  lcCurrStat = 'DISABLE'
  lcSMCmSt    = 'CWR'+RIGHT(gfTempName(),7)  && hold the Setup window name
  *E300293,1 end
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  SELECT SYCINT
  SELECT DISTINCT sycint.ccont_desc,sycint.ccont_code ;
         FROM (gcSysHome+"SYCINT") ;
         INTO ARRAY laCountry
  IF _TALLY = 0
    laCountry = " "
  ENDIF
*--hesham el-sheltawi remarked  
*  DO CASE
*    CASE _DOS
*      lcCountry = ""
*    CASE _WINDOWS
*      ibCountry = 0
*      puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[16]),gcIntCont,laData[16]),sycint.ccont_code,'CCONTCODE')     
*  ENDCASE
  
  SELECT sycComp
  lcPrompt    = lcTSelect
  lcTempSetup = gfTempName()
*  SELECT SYCSETUP
*  =AFIELDS(laFilStruct)
*  CREATE TABLE (gcWorkDir+lcTempSetup) FROM ARRAY laFilStruct
*  INDEX ON CCOMP_ID+STR(NVARPOS) TAG NVARPOS
*  SELECT (lcBaseFile)
ENDIF

*--hesham el-sheltawi    start
puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[16]),gcIntCont,laData[16]),sycint.ccont_code,'CCONTCODE')
*--hesham el-sheltawi    end

lcDirStat   = IIF(laScrMode[4],"ENABLE","DISABLE")
lcPrntStat  = IIF(laScrMode[4],"ENABLE","DISABLE")

*E300293,1 START defintion for the windows array
laWndObj[1,1]  = gcBaseWind
laWndObj[1,2]  = 'LADATA[1]'
laWndObj[1,3]  = 'pbSetup'

laWndObj[2,1]  = 'GWCCONTRL1'
laWndObj[2,2]  = 'PBTOP'
laWndObj[2,3]  = 'PBCLS'

*laWndObj[3,1]  = lcSMCmSt
*laWndObj[3,2]  = 'LADATA[18]'
*laWndObj[3,3]  = 'pbClose'

*E300293,1 End defintion for the windows array

*E300683,4 Call *.SPR from screens directory
* DO SMCmInf.SPR 
DO (gcScrDir + gcWinAppl + '\SMCmInf.SPR')
*E300683,4 end  
IF glQuitting
  *E300692,5 Close opened files if any
  IF USED('ACCOD')
    USE IN ACCOD
  ENDIF  
  IF USED('FISHD')
    USE IN FISHD
  ENDIF  
  *E300692,5 end

  RELEASE WINDOW (lcSMCmSt)
  IF USED(lcTempSetup)
    USE IN (lcTempSetup)
  ENDIF
  ERASE (gcWorkDir+lcTempSetup+'.DBF')
  ERASE (gcWorkDir+lcTempSetup+'.FPT')  
  ERASE (gcWorkDir+lcTempSetup+'.CDX')  
ENDIF

*!**************************************************************************
*! 
*!      Procedure: lpShow
*!
*!**************************************************************************

PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[1]
    llView = .F.
    DO CASE
      CASE _DOS
        lcCountry = ""
      CASE _WINDOWS
*        ibCountry = 0
        puCountry = LOOKUP(SYCINT.CPARt1LAB,gcIntCont,sycint.ccont_code,'CCONTCODE')     
    ENDCASE
    lcPrntName = " "
    DECLARE laModules[1],laAllMod[1]
    STORE " " TO laModules,laAllMod
    STORE 'DISABLE' TO lcMultCCnt,lcMultCurr
    =lfShowCurr()
    lcOldCmp=laData[11]   && Assign the company directory to variable
    SHOW GET pbInsSet   DISABLE
    SHOW GET pbSetup   DISABLE
    SHOW GET laData[11] DISABLE
    SHOW GET pbDatDir   DISABLE
    SHOW GET laData[14] DISABLE
    lcCurrStat = 'DISABLE'
    SHOW GET laData[17] DISABLE
    SHOW GET ibAddrType DISABLE
    SHOW GET ibCurrKey DISABLE
    IF USED(lcTempSetup)
      SELECT (lcTempSetup)
      ZAP
    ENDIF
    SELECT (lcBaseFile)
  CASE laScrMode[2]
    lcCurrStat = 'DISABLE'
    llView     = .F.
    lcOldCmp   = laData[11]   && Assign the company directory to variable
    lcOldMod   = laData[15]
    lnComRec   = RECNO()
    lcPrntName = IIF(EMPTY(laData[14]),'',;
                     LOOKUP(sycComp.cCom_name ,ALLTRIM(laData[14]),;
                            sycComp.cComp_id,'cComp_id'))
    IF lnComRec > 0 .AND. lnComRec <=RECCOUNT()
      GO lnComRec
    ENDIF
    SHOW GET laData[11] DISABLE
    SHOW GET pbDatDir   DISABLE
    SHOW GET laData[14] DISABLE
    IF EMPTY(laData[14])
      ** If this company is a parent compnay. **

      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" ;
             INTO ARRAY laAllMod
    ELSE
      ** If this company is a child company. **
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" .AND. !lparntmdl ;
             INTO ARRAY laAllMod
    ENDIF
    

    FOR lnCount1 = 1 TO ALEN(laCountry,1)
      IF ALLTRIM(laData[16]) = ALLTRIM(laCountry[lnCount1,2])
        DO CASE
          CASE _DOS
            lcCountry = laCountry[lnCount1,1]
          CASE _WINDOWS
*-- hesham el-sheltawi          
*            ibCountry = lnCount1
             puCountry = LOOKUP(SYCINT.CPARt1LAB,IIF(EMPTY(laData[16]),gcIntCont,laData[16]),sycint.ccont_code,'CCONTCODE')     
        ENDCASE
        EXIT
      ENDIF
    ENDFOR
    STORE 'DISABLE' TO lcMultCCnt,lcMultCurr
    =lfShowCurr()
    *E300633,1 Hesham El_Sheltawi (Start)
    *E300633,1 remove the dealing with sycsetup file to make the setup file
    *E300633,1 on the company level
    *IF SEEK(laData[1],'SYCSETUP')
      *SELECT *,'S' AS CSTATUS,RECNO() AS NRECNO FROM (gcSysHome+'SYCSETUP') ;
           WHERE CCOMP_ID+STR(NVARPOS)=laData[1];
           INTO TABLE (gcWorkDir+lcTempSetup)
    *ELSE  
       *SELECT laData[1] AS CCOMP_ID,*,'A' AS CSTATUS,0 AS NRECNO FROM (gcSysHome+'SYCCONFG') ;
           INTO TABLE (gcWorkDir+lcTempSetup)
    *ENDIF  
    *SELECT *,'S' AS CSTATUS,RECNO() AS NRECNO FROM (ALLT(laData[11])+'SETUPS') ;
      INTO TABLE (gcWorkDir+lcTempSetup)
    *INDEX ON STR(NVARPOS) TAG NVARPOS
    *E300633,1 Hesham El_Sheltawi (End)
    SELECT (lcBaseFile) 
    *E301469,1 AMH Disable edit button for History Companies [Start]
    IF GfGetMemVar('LLHIST',laData[1])
      SHOW GET pbEdt   DISABLE
    ENDIF
    *E301469,1 AMH [End]
  CASE laScrMode[3]
    lcCurrStat = IIF(EMPTY(laData[17]),'ENABLE','DISABLE')
    lnComRec   = RECNO()
    
    lcPrntName = IIF(EMPTY(laData[14]),'',;
                     LOOKUP(sycComp.cCom_name ,ALLTRIM(laData[14]),;
                            sycComp.cComp_id,'cComp_id'))
    *E300692,5 Get data dir path
    *E301098,1 Use gfGetDataDir
    *lcDataDir  = ALLTRIM(IIF(EMPTY(laData[14]), laData[11], SYCCOMP.cCom_DDir))
    lcDataDir  = gfGetDataDir(ALLTRIM(IIF(EMPTY(laData[14]), laData[11], SYCCOMP.cCom_DDir)))
    *E301098,1 end
    
    =lfOpnFiles('ACCOD','ACCSEGNO')
    *E300692,5 end
    IF lnComRec > 0 .AND. lnComRec <=RECCOUNT()
      GO lnComRec
    ENDIF
    SHOW GET laData[11] DISABLE
    SHOW GET pbDatDir   DISABLE
    SHOW GET laData[14] DISABLE
    
    IF EMPTY(laData[14])
      ** If this company is a parent compnay. **
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" ;
             INTO ARRAY laAllMod
    ELSE
      ** If this company is a child company. **

      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. cApp_Id <> "SY" .AND. !lparntmdl ;
             INTO ARRAY laAllMod
    ENDIF
    *B601531,1 M.H Begin.
    SHOW GET ibAddrType ENABLE
    *B601531,1 M.H End.
    *SICO START
    *lcMultCCnt=IIF(laData[18],'DISABLE','ENABLE') 
    *lcMultCurr=IIF(laData[18],'ENABLE','DISABLE')
    *SICO END
    =lfShowCurr()
  CASE laScrMode[4]
    llNewSetup = .T.
    DO CASE
      CASE _DOS
        lcCountry = laCountry[1,1]
      CASE _WINDOWS
         *--hesham el-sheltawi      
         *ibCountry = 1
         puCountry = LOOKUP(SYCINT.CPARt1LAB,gcIntCont,sycint.ccont_code,'CCONTCODE')     
    ENDCASE
    laData[8]  = LOOKUP(SYCINT.cCont_Desc,gcIntCont,sycint.ccont_code,'CCONTCODE')
    *B601484,1 M.H Begin.
    laData[16] = gcIntCont
    *B601484,1 M.H End.
    laData[17] = LOOKUP(SYCINT.cCurrCode,gcIntCont,sycint.ccont_code,'CCONTCODE')
    lcPrntName = " "
    ** Build the string of the company directory **
    laData[11]=gcAllCmp+IIF(RIGHT(ALLTRIM(gcAllCmp),1)='\','','\')+;
                            UPPER(ALLTRIM(laData[1]))+'\'
    SHOW GET laData[8]
    SHOW GET laData[11] ENABLE
    SHOW GET pbDatDir   ENABLE
    SHOW GET pbInsSet   DISABLE
    SHOW GET pbSetup   DISABLE    
    
    GO TOP IN SYCCOMP
    IF EOF('SYCCOMP')
      SHOW GET laData[14] DISABLE
    ELSE
      SHOW GET laData[14] ENABLE
    ENDIF  
    lcCurrStat = 'ENABLE'
    SHOW GET laData[17] ENABLE
    SHOW GET ibAddrType ENABLE
    SHOW GET ibCurrKey ENABLE        
    STORE 'ENABLE' TO lcMultCCnt

    *lcMultCurr=IIF(laData[18],'ENABLE','DISABLE')
    =lfShowCurr()    
    *E300633,1 Hesham El_Sheltawi (Start)
    *E300633,1 remove the dealing with sycsetup file to make the setup file
    *E300633,1 on the company level
    *SELECT laData[1] AS CCOMP_ID,*,'A' AS CSTATUS,0 AS NRECNO FROM (gcSysHome+'SYCCONFG') ;
        INTO TABLE (gcWorkDir+lcTempSetup)
    *INDEX ON CCOMP_ID+STR(NVARPOS) TAG NVARPOS    
    *E300633,1 Hesham El_Sheltawi (End)
    *SELECT (lcBaseFile)     
ENDCASE
*Hesham (start)
*B603737,1 if sort the modules selected to be installed to make the communication module
*B603737,1 first module if selected
=lfSortMod()
*Hesham (end)

SELECT (lcBaseFile)     
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
***Valid function of the company ID...
*
FUNCTION lfvData_1
IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13)
  =gfSeekRec() 
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvDatDir
*!
*!**************************************************************************
** Valid function for the push button < Data Dictionary >...
*
FUNCTION lfvDatDir

*YMO Company
*lcOldCmp   = laData[11]
*lcNewPath  = GETDIR()
*laData[11] = IIF(!EMPTY(lcNewPAth),lcNewpath,lcOldCmp)
*SHOW GET laData[11]
*_CUROBJ = OBJNUM(laData[11])
*IF ALLTRIM(UPPER(lcOldCmp)) <> ALLTRIM(UPPER(laData[11]))
*  =gfUpdate()
*ENDIF
*DO lfvData_11

*!**************************************************************************
*!
*!      Function: lfwData_11
*!
*!**************************************************************************
** When function for laData[11] field {company path}
*
FUNCTION lfwData_11
lcOldCmp = UPPER(ALLTRIM(laData[11]))

*!**************************************************************************
*!
*!      Function: lfvData_11
*!
*!**************************************************************************
** Valid function for laData[11] field {company path}
*
FUNCTION lfvData_11

IF !EMPTY(laData[11]) 
  ** Check for the directory if used by onther company ** 
  IF UPPER(ALLTRIM(lcOldCmp)) <> UPPER(ALLTRIM(laData[11]))
    lnRecCmp= RECNO("SYCCOMP")
    GO TOP
    LOCATE FOR cComp_ID<>laData[1] .AND. ;
               UPPER(ALLTRIM(cCom_Ddir)) == UPPER(ALLTRIM(laData[11]))
    IF lnRecCmp > 0 .AND. lnRecCmp <= RECCOUNT("SYCCOMP")
      GOTO lnRecCmp  
    ENDIF  

    IF FOUND()
      ** Path ð is in use by another company. **
      ** You cannot use this path...
      ** <  Ok  > **
      =gfModalGen("QRM00032B00000","DIALOG",+" "+laData[11])
      laData[11] = lcOldCmp
      SHOW GET laData[11]  
      _CUROBJ=OBJNUM(laData[11])
      RETURN
    ENDIF  
  ENDIF

  ** The directory is created or used **
  IF gfValdPath(ALLTRIM(laData[11]))
    IF UPPER(ALLTRIM(laData[11])) <> UPPER(ALLTRIM(lcOldCmp))
      llFlagValid = .T. 
      ** Dir ð already exists. Use it anyway? **
      ** < Yes > - <  No  > **
      IF gfModalGen("QRM00033B00006","ALERT",+" "+laData[11]) =  2 
        laData[11] = lcOldCmp
        SHOW GET laData[11]
        _CUROBJ=OBJNUM(laData[11])
        RETURN
      ENDIF  
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_14
*!
*!**************************************************************************
** When function for the laData[14] {parent company}...
*
FUNCTION lfwData_14

lcOldPrnt = laData[14]
lcOldPrnN = lcPrntName

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
** Valid function for the laData[14] {parent company}...
*
FUNCTION lfvData_14
IF laData[14] = lcOldPrnt
  RETURN
ENDIF  
  
IF EMPTY(laData[14]) .AND. LASTKEY() = 13
  lcPrntName = ""
  =lfRefresh()
  RETURN
ENDIF

IF laScrMode[3]
  lnComRec = RECNO()
ENDIF

IF LASTKEY() = 13
  IF SEEK(laData[14],"SYCCOMP")
    IF !EMPTY(sycComp.cCompPrnt)
      ** Company {laData[14]} is already a child company. **
      ** You cannot choose a child company as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00165B00000","DIALOG",;
                  "( "+laData[14]+"-"+ALLTRIM(sycComp.cCom_name)+" )")
      laData[14] = lcOldPrnt
      SHOW GET laData[14]
      IF laScrMode[3] .AND. lnComRec > 0 .AND. lnComRec <= RECCOUNT()
        GO lnComRec
      ENDIF
      RETURN
    ENDIF
    
    IF laData[14] = laData[1]
      ** You cannot choose the current company **
      ** to be a parent to itself...
      ** <  Ok  > **
      =gfModalGen("TRM00166B00000","DIALOG")
      laData[14] = lcOldPrnt
      IF laScrMode[3] .AND. lnComRec > 0 .AND. lnComRec <= RECCOUNT()
        GO lnComRec
      ENDIF
      RETURN
    ENDIF
    
    *E300692,5 Open ACCOD instead of SYCACCOD
    *SELECT SYCACCOD
    *B600906,1 MAN 03/28/96 Do not display a message if the P.Comp field has ? 
    *IF !SEEK(laData[14])
    *IF IIF(AT('?',laData[14]) > 0 ,!SEEK(laData[14]),.F.)
    
    *E301098,1 Use gfGetDataDir
    *lcDataDir  = ALLTRIM(SYCCOMP.cCom_DDir)
    lcDataDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
    *E301098,1 end
    
    =lfOpnFiles('ACCOD','ACCSEGNO')
    *E300789,1 Hesham (Start)
    *IF IIF(AT('?',laData[14]) > 0 ,!SEEK(laData[14],'ACCOD'),.F.)
    IF IIF(AT('?',laData[14]) > 0 ,!SEEK('','ACCOD'),.F.)    
    *E300789,1 Hesham (End) 
    *E300692,5 end
      ** This company does not have an account **
      ** code structure.  You cannot choose it **
      ** as a parent company...
      ** <  Ok  > **
      =gfModalGen("TRM00198B00000","DIALOG")
      laData[14] = lcOldPrnt
      SHOW GET laData[14]
      SELECT SYCCOMP
      RETURN
    ENDIF
    lcPrntName = sycComp.cCom_name
  ELSE
    SELECT sycComp
    SET FILTER TO EMPTY(sycComp.cCompPrnt) .AND. ;
        sycComp.cComp_Id <> laData[1]

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow    = lcBrFields
    lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"
    lcOld_ttl     = lcFile_ttl
    lcFile_ttl    = "Company information"
    laCompInfo[1] = laData[14]
    laCompInfo[2] = lcPrntName

    *=gfBrows(.F.,"cComp_id,cCom_name","laCompInfo")
    IF gfBrows(.F.,"cComp_id,cCom_name","laCompInfo")    
      IF laData[14] = laCompInfo[1] 
        laData[14]  = lcOldPrnt
      ELSE
        laData[14] = laCompInfo[1]
        lcPrntName = laCompInfo[2]      
      ENDIF
      *E300692,5 Open ACCOD instead of SYCACCOD
      *SELECT SYCACCOD
      *IF !SEEK(laData[14])
      =SEEK(laData[14], 'SYCCOMP')
      *E301098,1 Use gfGetDataDir
      *lcDataDir  = ALLTRIM(SYCCOMP.cCom_DDir)
      lcDataDir  = gfGetDataDir(ALLTRIM(SYCCOMP.cCom_DDir))
      *E301098,1 end
      
      =lfOpnFiles('ACCOD','ACCSEGNO')
      *E300789,1 Hesham (Start)
      *IF !SEEK(laData[14],'ACCOD')
      IF !SEEK("",'ACCOD')
      *E300789,1 Hesham (End)
      *E300692,5 end
        ** This company does not have an account **
        ** code structure.  You cannot choose it **
        ** as a parent company...
        ** <  Ok  > **
        =gfModalGen("TRM00198B00000","DIALOG")
        laData[14] = lcOldPrnt
        lcPrntName = lcOldPrnN
        SELECT SYCCOMP
      ENDIF
    ELSE
      laData[14] = lcOldPrnt
    ENDIF
    lcBrFields    = lcSaveBrow 
    lcFile_ttl    = lcOld_ttl
  ENDIF
ELSE
  laData[14] = lcOldPrnt
ENDIF

SELECT SYCCOMP
SET FILTER TO
IF laScrMode[3] .AND. lnComRec > 0 .AND. lnComRec <= RECCOUNT()
  GO lnComRec
ENDIF

SHOW GET laData[14]

=lfRefresh()

IF lcOldPrnt <> laData[14]

  IF EMPTY(laData[14])
    ** If this company is a parent compnay. **
    
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" ;
           INTO ARRAY laAllMod

  ELSE
    ** If this company is a child company. **
    SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
           FROM (gcSysHome+"SYDAPPL") ;
           WHERE cApp_Id <> "SM" .AND. ;
                 cApp_Id <> "SY" .AND. ;
                 !lparntmdl ;
           INTO ARRAY laAllMod
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
FUNCTION lfvData_16

DO CASE
  CASE _DOS
    laData[16] = gfActPop(5,21,11,43,'laCountry',2,1,@lcCountry)
    =lfRefresh()
  CASE _WINDOWS
    laData[16] = laCountry[ibCountry,2]
    SHOW GET ibComp
ENDCASE

IF EMPTY(laData[8])
  laData[8] = lcCountry
  SHOW GET laData[8]
ENDIF

*!**************************************************************************
*!
*!      Function: lpSavScr
*!
*!**************************************************************************
* Procedure to save the current company record...
*
FUNCTION lpSavScr
** Check if the company name is empty or not...
IF EMPTY(laData[2])
  ** You cannot save this company without name. **
  =gfModalGen("TRM00171B00000","DIALOG")
  llCSave = .F.
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

** Get the path...
laData[11] = IIF(RIGHT(ALLTRIM(laData[11]),1)<>'\',;
                  UPPER(ALLTRIM(laData[11]))+'\',;
                  UPPER(ALLTRIM(laData[11])))

** See if this company or not in the company file **
** for defining the menu popup comany bar...
SELECT syccomp
lnComRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
GO TOP
IF EOF()
  llFirstComp = .T.
ELSE
  llFirstComp = .F.
ENDIF

IF lnComRec <> 0
  GOTO lnComRec
ENDIF  

** Save this company record in the company file. **


SELECT sycComp
*B804186,1 NAD 05/06/2001 (Start)
IF laScrMode[3]
 SEEK laData[1]
ENDIF
*B804186,1 NAD 05/06/2001 (End)
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()
*E300633,1 Hesham El_Sheltawi (Start)
*E300633,1 remove the dealing with sycsetup file to make the setup file
*E300633,1 on the company level
*SELECT SYCSETUP
*=gfTmp2Mast("SYCSETUP",lcTempSetup,'Saving company settings...')  
*E300633,1 Hesham El_Sheltawi (End)
** This process will be execute if add mode only...
IF laScrMode[4] 
  lcSavDef = FULLPATH(SET("DEFAULT"))

  *B601309,1 Reham On 12/04/96    *** Begin ***
  *B601309,1 Check if the dbfs directory exist if it was first intallation.
  lcDbfPath = IIF(RIGHT(ALLTRIM(laData[11]),1)='\',;
                SUBSTR(ALLTRIM(laData[11]) , 1 , LEN(ALLTRIM(laData[11])) - 3),;
                SUBSTR(ALLTRIM(laData[11]) , 1 , LEN(ALLTRIM(laData[11])) - 2))
  IF !gfValdPath(lcDbfPath)
    lcDbfPath = IIF(RIGHT(ALLTRIM(lcDbfPath),1)='\',;
                SUBSTR(ALLTRIM(lcDbfPath),1,LEN(ALLTRIM(lcDbfPath))-1),;
                ALLTRIM(lcDbfPath))
    *B601309,1 Create the DBFS directory on its right place if not exist.
    DO CASE
      CASE _DOS
        !MD &lcDbfPath
      CASE _WINDOWS
        =gfMkDir(lcDbfPath)
    ENDCASE
  ENDIF
  *B601309,1 Reham On 12/04/96    *** End   ***
    
  ** If the path is not found, create it. **
  IF !gfValdPath(laData[11])
    lcNewPath =IIF(RIGHT(ALLTRIM(laData[11]),1)='\',;
                   SUBSTR(ALLTRIM(laData[11]),1,;
                   LEN(ALLTRIM(laData[11]))-1),;
                   ALLTRIM(laData[11]))
    DO CASE
      CASE _DOS
        !MD &lcNewPath
      CASE _WINDOWS
        =gfMkDir(lcNewPath)
    ENDCASE
  ENDIF
  *E300633,1 Hesham El_Sheltawi (Start)
  *E300633,1 Create the company Setups file in the comp. data directory
    lcAliasSel = SELECT()
    *E301077,78 Hesham (Start)
    =gfOpenFile(gcSysHome+'SYCCONFG')
    *SELECT SYCCONFG    
    *E301077,78 Hesham (End)
    COPY TO (ALLT(laData[11])+'\SETUPS') FOR EMPTY(CAPP_ID) WITH CDX
    SELECT (lcAliasSel)
  *E300633,1 Hesham El_Sheltawi (End)
  *E300645,1 Hesham El-Sheltawi (Start)
  *E300645,1 create the system data files for the company
    lcAliasSel = SELECT()
    =lfInstall('SY')   
    SELECT (lcAliasSel)  
  *E300645,1 Hesham El_Sheltawi (End)  
  SET DEFAULT TO &lcSavDef
  
  ** Add this company title in the menu **
  ** popup of the companies...
  IF llFirstComp
    lnBarNo = CNTBAR('_COMPANIES')
  ELSE
    lnBarNo = CNTBAR('_COMPANIES')+1
  ENDIF
  DEFINE BAR lnBarNo OF _COMPANIES PROMPT laData[1]+'-'+ALLTRIM(laData[2])

  lcComp_Id = '"'+laData[1]+'"'
  ON SELECTION BAR lnBarNo OF _COMPANIES ;
              DO gpChangCom WITH &lcComp_Id

  glCmpCreat = .T.
  
  ** Go to the view mode so we can open the **
  ** related program to add a company...
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS

  ** If this company is not a child company. **
  IF EMPTY(laData[14])
    ** Do you want to add Fiscal year for Company ð Now ? **
    ** < Yes > - <  No  > **
    IF gfModalGen("QRM00066B00006","DIALOG",laData[1]) = 1
      ** Save the current environment in the static file. **
      lcCurrWind = gcBaseWind
      =gfStatic()
      lcWindow   = UPPER(lcBaseWind)
      llSavFis   = .F.
      glFirsTime = .T.

      gcBaseWind ='AWRSMFSYER'

      ** Call the fiscal year program to add the **
      ** fiscal years for  this company  and its **
      ** periods and holidays...
      lcAppName  = gcAppHome+'SM.APP'
      
      *E300683,4 Pass a parameter to SM.PRG
      *DO (lcAppName) WITH 'SMFSYER WITH "&laData[1]"'    
      DO (lcAppName) WITH 'SMFSYER',"'"+laData[1]+"'"    
      *E300683,4 end
      
      *E301077,78 Hesham (Start)
      =gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID')         
      *E301077,78 Hesham (End)

      lcSavExact = SET('EXACT')
      SET EXACT ON

      llTempFis = llSavFis
      
      IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
        ** Restore data for the company program from static file. **
        RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
      ENDIF  
      SET EXACT &lcSavExact
      
      *B602658,6 Reposition file pointer in SYCCOMP after restoring laData[1]
      =SEEK(laData[1],'SYCCOMP')
      *B602658,6 end

      llSavFis = llTempFis
      glQuitting = .F.
      gcBaseWind = lcCurrWind
    ENDIF
     SELECT sycComp
    
    ** Refresh the current year and current period **
    ** that displayed in the screen as say fields. **
    laData[12] = syccomp.ccurr_yer
    laData[13] = syccomp.ccurr_prd
    SHOW GETS
    
    ** Do you want to add account code strucure for Company ð Now? **
    ** < Yes > - <  No  > **
    IF gfModalGen("QRM00172B00006","DIALOG",laData[1]) = 1
      ** Save the current environment in the static file. **
      lcCurrWind = gcBaseWind
      =gfStatic()
      lcWindow   = UPPER(lcBaseWind)
      llSavAcc   = .F.
      glFirsTime = .T.

      gcBaseWind ='AWDSMACCOD'
      
      ** Call the account code strucure program **
      ** to add the account code struc. for the **
      ** added comapny...
      
      lcAppName  = gcAppHome+'SM.APP'
      *E300683,4 Pass a parameter to SM.PRG
      *DO (lcAppName) WITH 'SMACCOD WITH "&laData[1]"'    
      DO (lcAppName) WITH 'SMACCOD',"'"+laData[1]+"'"    
      *E300683,4 end
      
      *E301077,78 Hesham (Start)
      =gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID')         
      *E301077,78 Hesham (End)

      lcSavExact = SET('EXACT')
      SET EXACT ON

      llTempAcc = llSavAcc
      
      IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
        ** Restore data for the company program from static file. **
        RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
      ENDIF  
      SET EXACT &lcSavExact
      
      *B602658,6 Reposition file pointer in SYCCOMP after restoring laData[1]
      =SEEK(laData[1],'SYCCOMP')
      *B602658,6 end

      llSavAcc = llTempAcc
      glQuitting = .F.
      gcBaseWind = lcCurrWind
    ENDIF
  ELSE
    ** This company is a child company for **
    ** company laData[14].  So this child **
    ** company will use the the parent company **
    ** fiscal year and account code structure...
    ** <  Ok  > **
    =gfModalGen("TRM00170B00000","DIALOG",;
                "("+laData[14]+"-"+ALLTRIM(lcPrntName)+")")
    llSavFis = .T.
    llSavAcc = .T.
    
    ** If the current company is a child company **
    ** It has to  have its  parent account  code **
    ** strucure...
    *E300692,5 Do not add an account code structure if the company has a parent
    *lcTime = gfGetTime()    && Get the current time.
    *DECLARE laStruc[1,1]    && Array to hold the data.
    *laStruc = " "
  
    *E300692,5 Change file name and path from SYCACCOD to ACCOD      
    *SELECT SYCACCOD
    *SELECT laData[1] AS CCOMP_ID,NACSSEGSZ,CACSMASK, ;
           CACSEGDES,NACSNOSEG,NACSSEGNO, ;
           NACSSIZE,CACSSHDES,CACSLGDES, ;
           .F. AS LACSUSED,gcUser_Id AS CADD_USER, ;
           DATE() AS DADD_DATE, ;
           lcTime AS CADD_TIME ;
    FROM (gcSysHome+"SYCACCOD") ;
    WHERE cComp_Id = laData[14] ;
    INTO ARRAY laStruc
    
    *E300692,5 Get data directory
    *lcDataDir = ALLTRIM(IIF(SEEK(laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, laData[11]))
    *E300692,5 Reposition file pointer
    *=SEEK(laData[1],'SYCCOMP')
    *SELECT laData[1] AS CCOMP_ID,NACSSEGSZ,CACSMASK, ;
           CACSEGDES,NACSNOSEG,NACSSEGNO, ;
           NACSSIZE,CACSSHDES,CACSLGDES, ;
           .F. AS LACSUSED,gcUser_Id AS CADD_USER, ;
           DATE() AS DADD_DATE, ;
           lcTime AS CADD_TIME  ;
      FROM (lcDataDir+"ACCOD")  ;
    INTO ARRAY laStruc

    *SELECT ACCOD
    *E300692,5 end  
    
    *APPEND FROM ARRAY laStruc ;
    FIELDS CCOMP_ID,NACSSEGSZ,CACSMASK, ;
           CACSEGDES,NACSNOSEG,NACSSEGNO, ;
           NACSSIZE,CACSSHDES,CACSLGDES, ;
           LACSUSED,CADD_USER,DADD_DATE, ;
           CADD_TIME
    *E300692,5 end       
  ENDIF
  
  SELECT sycComp

  ** Do you want to install the company modules now? **
  ** <  Yes  > - <  No  > **
  IF gfModalGen("QRM00177B00006","DIALOG") = 1
  
    ** Collect all the available modules in the system. **
    DECLARE laAllMod[1]
    laAllMod = " "
    
    IF EMPTY(laData[14])
      ** If this company is a parent compnay. **
      
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. ;
                   cApp_Id <> "SY" ;
             INTO ARRAY laAllMod

    ELSE
      ** If this company is a child company. **
      
      SELECT "  "+cApp_Id+" "+cApp_name+" +"+IIF(lSetReq,"T","F") ;
             FROM (gcSysHome+"SYDAPPL") ;
             WHERE cApp_Id <> "SM" .AND. ;
                   cApp_Id <> "SY" .AND. ;
                   !lparntmdl ;
             INTO ARRAY laAllMod
    ENDIF
    *Hesham (start)
    *B603737,1 if sort the modules selected to be installed to make the communication module
    *B603737,1 first module if selected
    =lfSortMod()
    *Hesham (end)
    IF _TALLY > 0
      
      ** Call the installtion and setup modules screen. **
      llSavMod  = .T.
      lnMaxScrn = IIF(_DOS,78,68.750)
      lnColPos  = 22
      llView    = .T.
      lcPrompt  = lcTSelect
      *E300683,4 Call *.SPR from screens directory
      * DO SMINSET.SPR 
      DO (gcScrDir + gcWinAppl + '\SMINSET.SPR')
      *E300683,4 end  
    ELSE
      ** There is no modules to install its files. **
      ** <  Ok  > **
      =gfModalGen("TRM00178B00000","DIALOG")
    ENDIF
  ELSE
    ** Without installion you cannot access **
    ** any module in this company. **
    ** <  Ok  > **
    =gfModalGen("TRM00182B00000","DIALOG")
  ENDIF
  llCSave = .F.
ENDIF

SELECT SYCCOMP
IF lnComRec <> 0
  GOTO lnComRec
ENDIF  

IF llFirstComp
  ** Do you want to set this company as a system default company. **
  ** <  Yes  > - <  Cancel  > **
  IF gfModalGen("TRM00199B00006","DIALOG") = 1
    =gfOpenFile(gcSysHome+'SYCINST')
    *SELECT SYCINST
    REPLACE sycinst.cinsdfcom WITH laData[1]
    SELECT SYCCOMP
  ENDIF
  
  ** Do you want to select the comany now. **
  ** <  Yes  > - <  Cancel  > **
  IF gfModalGen("TRM00191B00006","DIALOG") = 1
    =gpChangCom(laData[1],.F.,.F.)
  ENDIF
ENDIF

IF laData[1] = gcAct_Comp
  gcCom_Name = ALLTRIM(sycComp.cCom_Name)
  
  *E301098,1 Use gfGetDataDir
  *gcDataDir  = ALLTRIM(sycComp.cCom_dDir)
  gcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_dDir))
  *E301098,1 end
  
  gcComp_Mdl = ALLTRIM(sycComp.mComp_Mdl)
  gcComp_Lvl = IIF(EMPTY(sycComp.ccompprnt),'P','C')
  gcPrnt_Cmp = IIF(!EMPTY(sycComp.ccompprnt),;
                   sycComp.ccompprnt,sycComp.ccomp_Id)
*B601281,1 M.H Begin.
  gcContCode = ALLTRIM(laData[8])
*B601281,1 M.H End.
  =gfCompSets(ALLTRIM(syccomp.ccont_code))
ENDIF  
*-- end of lpsavScr. Setups
  
*!**************************************************************************
*!
*!      Function: lpDelScr
*!
*!**************************************************************************
** Procedure to delete the current company record...
*
FUNCTION lpDelScr

** Check if any user is using this company. **
SELECT SYUSTATC
GO TOP
LOCATE FOR ALLTRIM(cComp_id) = ALLTRIM(laData[1])
*B602064,1 MAN 08/02/89 Added RLOCK() to insure that the record is currently
*B602064,1 MAN          used
IF FOUND() .AND. RLOCK()
  ** Company is in use by onther user, Can't Deleted. **
  ** <  Ok  > **
  =gfModalGen("INM00059B00000","ALERT",+" "+laData[1])
  SELECT sycComp
  RETURN
ENDIF


SELECT sycComp
** Check if this company is a parent company to any other companies...
lnComRec = RECNO()

LOCATE FOR sycComp.cCompPrnt = laData[1]
IF FOUND()
  ** This company is a parent company for 1 or more company. **
  ** You cannot delete it...
  ** <  Ok  > **
  =gfModalGen("TRM00167B00000","DIALOG")
  IF lnComRec <= RECCOUNT()
    GO lnComRec
  ENDIF
  RETURN
ENDIF

IF lnComRec <= RECCOUNT()
  GO lnComRec
ENDIF

** Check if this company is the default company for the system...
=gfOpenFile(gcSysHome+'SYCINST')
*SELECT SYCINST

IF SYCINST.cInsdfcom = laData[1]
  ** This company is the system default company. **
  ** < Clear default > - < Cancel deletion > **
  IF gfModalGen("TRM00168B00028","DIALOG") = 1
    REPLACE SYCINST.cInsdfcom WITH "  "
  ELSE
    SELECT sycComp
    RETURN
  ENDIF
ENDIF

** Check if this company is the default company for any user...
SELECT SYUUSER
LOCATE FOR cUsr_dcom = laData[1]
IF FOUND()
  ** This company is the default company for one or more than user. **
  ** < Clear default > - < Cancel deletion > **
  IF gfModalGen("TRM00169B00028","DIALOG") = 1
    SCAN FOR cUsr_dcom = laData[1]
      REPLACE SYUUSER.cUsr_dcom WITH "  "
    ENDSCAN
  ELSE
    SELECT sycComp
    RETURN
  ENDIF
ENDIF

DECLARE laTally[1]
laTally = 0

*E300643,1 Remove SYCCODES from the next  query
*SELECT COUNT(*) AS nTemp ;
*       FROM (gcSysHome+"SYCCODES") ;
*      WHERE SYCCODES.CCOMP_ID+SYCCODES.CRLTFIELD+SYCCODES.CFLD_NAME = ;
 *           laData[1]+"N" .AND. LLOK_STAT ;
*UNION ;
*SELECT COUNT(*) AS nTemp ;
*       FROM (gcSysHome+"SYCACCOD") ;
*       WHERE SYCACCOD.CCOMP_ID = laData[1] .AND. SYCACCOD.LLOK_STAT ;
*UNION ;
*SELECT COUNT(*) AS nTemp ;
*       FROM (gcSysHome+"SYCFISHD") ;
*       WHERE CCOMP_ID+CFISFYEAR = laData[1] .AND. SYCFISHD.LLOK_STAT ;
*       INTO ARRAY laTally ORDER BY nTemp DESC

*E300692,5 Change files name and path from SYCACCOD and SYCFISHD to ACCOD and FISHD
*SELECT COUNT(*) AS nTemp ;
       FROM (gcSysHome+"SYCACCOD") ;
       WHERE SYCACCOD.CCOMP_ID = laData[1] .AND. SYCACCOD.LLOK_STAT ;
UNION ;
SELECT COUNT(*) AS nTemp ;
       FROM (gcSysHome+"SYCFISHD") ;
       WHERE CCOMP_ID+CFISFYEAR = laData[1] .AND. SYCFISHD.LLOK_STAT ;
       INTO ARRAY laTally ORDER BY nTemp DESC

*E300692,5 Get data directory

*E301098,1 Use gfGetDataDir
*lcDataDir = ALLTRIM(IIF(SEEK(laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, laData[11]))
lcDataDir = gfGetDataDir(ALLTRIM(IIF(SEEK(laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, laData[11])))
*E301098,1 end

*E300692,5 Reposition file pointer
=SEEK(laData[1],'SYCCOMP')
_TALLY = 0
IF FILE(lcDataDir+"ACCOD")
  *E300789,1 Hesham (Start)
  *SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"ACCOD") ;
         WHERE ACCOD.CCOMP_ID = laData[1] .AND. ACCOD.LLOK_STAT 
  SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"ACCOD") ;
         WHERE ACCOD.LLOK_STAT 
  *E300789,1 Hesham (End)
ENDIF 
IF _TALLY = 0 .AND. FILE(lcDataDir+"FISHD")       
  *E300789,1 Hesham (Start)
  *SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"FISHD") ;
         WHERE CCOMP_ID+CFISFYEAR = laData[1] .AND. FISHD.LLOK_STAT ;
         INTO ARRAY laTally ORDER BY nTemp DESC

  SELECT COUNT(*) AS nTemp ;
         FROM (lcDataDir+"FISHD") ;
         WHERE FISHD.LLOK_STAT ;
         INTO ARRAY laTally ORDER BY nTemp DESC

  *E300789,1 Hesham (END)
ENDIF         
*E300692,5 end       

*E300643,1 end

IF laTally[1] > 0
  ** You cannot delete this company.  There **
  ** is a user editing its system data now. **
  ** <  Ok  > **
  =gfModalGen("TRM00214B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Delete Company record from company file
SELECT SYCCOMP
*E301469,1 AMH empty M_Comp_Id setup of active company when deleting history company [Start]
IF GfGetMemVar('LLHIST',laData[1])
  IF SEEK(GfGetMemVar('M_Comp_Id',laData[1]))
    lcActDir = ALLTRIM(CCOM_DDIR)
    =gfOpenFile(lcActDir+'setups','modvar','SH','ACTSETUP',.T.)
    IF SEEK (CHR(255)+CHR(255)+'M_COMP_ID','ACTSETUP')
      SELECT ACTSETUP
      REPLACE MDATA_DEF WITH ''
    ENDIF
    =gfCloseFile('ACTSETUP')
    SELECT SYCCOMP
    SEEK laData[1]
  ENDIF
ENDIF
*E301469,1 AMH [End]
SCATTER MEMVAR MEMO BLANK
GATHER  MEMVAR MEMO 
DELETE
*B800241,1 check whether there is no companies are defined 
LOCATE
llFirstComp = EOF()
*--HESHAM
*E300633,1 Hesham El_Sheltawi (Start)
*E300633,1 remove the dealing with sycsetup file to make the setup file
*E300633,1 on the company level
*SELECT (lcTempSetup)
*REPLACE ALL CSTATUS WITH IIF(CSTATUS $ 'SM','D',CSTATUS)
*DELETE FOR CSTATUS='D'
*SELECT SYCSETUP
*=gfTmp2Mast("SYCSETUP",lcTempSetup,'Deleting company settings...')  
*E300633,1 Hesham El_Sheltawi (End)
** Remove Company from menu
FOR lnCount3 = 1 TO CNTBAR('_COMPANIES')
  IF SUBSTR(PRMBAR('_COMPANIES',GETBAR('_COMPANIES',lnCount3)),1,2)=ALLTRIM(laData[1])
    RELEASE BAR lnCount3 OF _COMPANIES
    glCmpCreat = .T.
    EXIT
  ENDIF
ENDFOR
*B800241,1 if no company are defined define bar in the company popup
*B800241,1 with prompt "No companies available"
IF llFirstComp
  DEFINE BAR 1 OF _COMPANIES;
     PROMPT "No companies available";
     COLOR SCHEME 3
ENDIF
** Delete company or remove files after chang company path
laScrMode    = .F.
laScrMode[1] = .T.

lcExactSt = SET('EXACT')
SET EXACT OFF

*E300643,1 Remove SYCCODES using
** Delete Company Codes from 'SYCCODES'
*SELECT SYCCODES
*SET ORDER TO CODES
*SCAN FOR CCOMP_ID+CCODE_NO = laData[1]
* REPLACE CCOMP_ID WITH "  "
*  DELETE
*ENDSCAN
*E300643,1 end

** Delete Company Sequence numbers from 'SYCSEQUN'
*E300663,1 Remove this lines for the changes we have made to SYCSEQUN [Begin]
*SELECT SYCSEQUN
*LOCATE FOR cComp_Id = laData[1]
*IF FOUND()
*  DELETE ALL FOR cComp_Id = laData[1]
*ENDIF
*E300663,1 Remove this lines for the changes we have made to SYCSEQUN [End]
  
*E300692,5 Remove deleting SYCFISHD, SYCFSPRD, SYCFSHLD, SYCACCOD
** Delete Company Periods from 'SYCFSPRD'
*SELECT SYCFSPRD
*SET ORDER TO TAG COMFYRPRDI
*IF SEEK(laData[1])
*  SCAN REST WHILE CCOMP_ID+CFISFYEAR+CFSPPRDID = laData[1]
*    DELETE
*  ENDSCAN
*ENDIF
** Delete Company Holidays from 'SYCFSHLD'
*SELECT SYCFSHLD
*SET ORDER TO TAG COMFYRHDAT
*IF SEEK(laData[1])
*  SCAN REST WHILE CCOMP_ID+CFISFYEAR+DTOS(DFSHHDATE) = laData[1]
*    DELETE
*  ENDSCAN
*ENDIF
** Delete Company Fiscal years from 'SYCFISHD'
*SELECT SYCFISHD
*SET ORDER TO TAG COMPFYEAR
*IF SEEK(laData[1])
*  SCAN REST WHILE CCOMP_ID+CFISFYEAR = laData[1]
*    DELETE
*  ENDSCAN
*ENDIF
** Delete the account code strucure for this company from 'SYCACCOD'.
*SELECT SYCACCOD
*SET ORDER TO COMPID
*SCAN FOR CCOMP_ID = laData[1]
*  REPLACE CCOMP_ID WITH "  "
*  DELETE
*ENDSCAN
*E300692,5 end

** Delete all the users privileges for this company. **
SELECT SYUUSRPR
SCAN FOR cComp_Id = laData[1]
  DELETE
ENDSCAN

** Delete all the tasks managers related to this company. **
SELECT SYUDTASK
SCAN FOR cComp_Id = laData[1]
  DELETE
ENDSCAN
    
SET EXACT &lcExactSt

*E300692,5 Remove Check for deleting files even if no modules are instaled
*IF !EMPTY(laData[15])
*E300692,5 end

** Delete company files or not. **
** Are you sure you want to delete the **
** company files in 'laData[11]'?...
** < Yes > - <  No  > **
IF gfModalGen("QRM00055B00006","ALERT",+ALLTRIM(laData[11])) = 1
  =lfDel_Fil()
ENDIF

*E300692,5 Remove ENDIF !EMPTY(laData[15])
*ENDIF
*E300692,5 end

SELECT SYCCOMP

*!**************************************************************************
*!
*!      Function: lpClsScr
*!
*!**************************************************************************
** Procedure to cancel the current company record...
*
FUNCTION lpClsScr

SELECT SYCCOMP
IF laScrMode[3] .OR. laScrMode[4]
  IF laScrMode[3]
    REPLACE mComp_Mdl WITH laData[15]
  ENDIF
  SCATTER FIELDS &lcScFields MEMO TO laData
ENDIF

*!**************************************************************************
*!
*!      Function: lfvInsSet
*!
*!**************************************************************************
** Valid function for the push button < Installation & Setup >
*
FUNCTION lfvInsSet

** Check if the account code strucure was **
** entered or not for current company...
*E300692,5 Change file name from SYCACCOD to ACCOD
*SELECT SYCACCOD
*E300692,5 Get data dir path

*E301098,1 Use gfGetDataDir
*lcDataDir  = ALLTRIM(IIF(SEEK(laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, laData[11]))
lcDataDir  = gfGetDataDir(ALLTRIM(IIF(SEEK(laData[14],'SYCCOMP'), SYCCOMP.cCom_DDir, laData[11])))
*E301098,1 end

=lfOpnFiles('ACCOD','ACCSEGNO')
=SEEK(laData[1], 'SYCCOMP')
SELECT ACCOD
*E300692,5 end

*llSavAcc = SEEK(laData[1])
*E300789,1 Hesham (Start)
*llSavAcc = SEEK(IIF(!EMPTY(laData[14]), laData[14], laData[1]))
llSavAcc = SEEK("")
*E300789,1 Hesham (End)
** Check if the fiscal years was entered **
** or not for current company...
*E300692,5 Change file name from SYCFISHD to FISHD
*SELECT SYCFISHD
=lfOpnFiles('FISHD','COMPFYEAR')
SELECT FISHD
*E300692,5 end

*B602658,6 Check if there are any records in the fiscal calendar
*B602658,6 after removing cComp_ID
*IF EMPTY(laData[14])
*  llSavFis = SEEK(laData[1])
*ELSE
*  llSavFis = SEEK(laData[14])
*ENDIF
llSavFis = SEEK("")
*B602658,6 end

** Flag to say I'm coming from edit mode. **
llSavMod = .F.
FOR lnCount4 = 1 TO ALEN(laAllMod,1)
  IF AT(SUBSTR(laAllMod[lnCount4],3,2),lcOldMod) > 0
    laAllMod[lnCount4] = SUBSTR(laAllMod[lnCount4],1,37)+;
                         "Installed    "+RIGHT(laAllMod[lnCount4],1)
  ELSE
    laAllMod[lnCount4] = SUBSTR(laAllMod[lnCount4],1,37)+;
                         "             "+RIGHT(laAllMod[lnCount4],1)
  ENDIF
ENDFOR

lnMaxScrn = IIF(laScrMode[2],IIF(_DOS,59,53),IIF(_DOS,78,68.750))
lnColPos  = IIF(laScrMode[2],13,22)
llView    = IIF(laScrMode[2],.F.,.T.)
lcPrompt  = IIF(LEFT(laAllMod[1,1],1) = " ",lcTSelect,lcTUnSelct)

*E300683,4 Call *.SPR from screens directory
* DO SMINSET.SPR 
DO (gcScrDir + gcWinAppl + '\SMINSET.SPR')
*E300683,4 end  
SELECT SYCCOMP

*!**************************************************************************
*!
*!      Function: lfvSel
*!
*!**************************************************************************
*
FUNCTION lfvSel

=lfvModules()

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF AT(SUBSTR(laAllMod[lnCont1],3,2),lcOldMod) > 0
    LOOP
  ELSE
    IF LEFT(laAllMod[lnCont1],1) = " "
      laAllMod[lnCont1] = lcDiamond + SUBSTR(laAllMod[lnCont1],2)
    ENDIF
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvNon
*!
*!**************************************************************************
*
FUNCTION lfvNon

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF LEFT(laAllMod[lnCont1],1) = lcDiamond
    laAllMod[lnCont1] = " " + SUBSTR(laAllMod[lnCont1],2)
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvInv
*!
*!**************************************************************************
*
FUNCTION lfvInv

FOR lnCont1 = 1 TO ALEN(laAllMod,1)
  IF AT(SUBSTR(laAllMod[lnCont1],3,2),lcOldMod) > 0
    LOOP
  ELSE
    IF LEFT(laAllMod[lnCont1],1) = " "
      laAllMod[lnCont1] = lcDiamond + SUBSTR(laAllMod[lnCont1],2)
    ELSE
      laAllMod[lnCont1] = " " + SUBSTR(laAllMod[lnCont1],2)
    ENDIF
  ENDIF
ENDFOR

SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfStatus
*!
*!**************************************************************************
*
FUNCTION lfStatus

IF LEFT(laAllMod[lsModules],1) = " "
  lcPrompt = lcTSelect
  SHOW GET pbSel,1 PROMPT lcTSelect
ELSE
  lcPrompt = lcTUnSelct
  SHOW GET pbSel,1 PROMPT lcTUnSelct
ENDIF

*!**************************************************************************
*!
*!      Function: lfwModules
*!
*!**************************************************************************
** When function for the list that hold all the modules **
** linked to this company in the SMINSET screen...
*
FUNCTION lfwModules

IF lsModules = 0 .OR. lsModules > ALEN(laAllMod,1) .OR. !llView
  RETURN
ENDIF

** Check if this module is installed before or not...
IF AT(SUBSTR(laAllMod[lsModules],3,2),lcOldMod) > 0
  SHOW GET pbIns   ENABLE
  SHOW GET pbUnIns ENABLE
  ** Check if setup required or not...
  IF RIGHT(laAllMod[lsModules],1) = "T"
    SHOW GET pbSet ENABLE
  ELSE
    SHOW GET pbSet DISABLE
  ENDIF
ELSE
  SHOW GET pbIns   ENABLE
  SHOW GET pbUnIns DISABLE
  SHOW GET pbSet   DISABLE
ENDIF

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvModules
*!
*!**************************************************************************
** Valid function for the list that hold all the modules **
** linked to this company in the SMINSET screen...
*
FUNCTION lfvModules

IF lsModules = 0 .OR. lsModules > ALEN(laAllMod,1) .OR. !llView
  RETURN
ENDIF

** If installed before. **
IF AT(SUBSTR(laAllMod[lsModules],3,2),lcOldMod) > 0
  ** This module has been installed before. **
  ** <  Ok  > **
  =gfModalGen("TRM00190B00000","DIALOG")
  RETURN
ELSE
  IF LEFT(laAllMod[lsModules],1) = " "
    laAllMod[lsModules] = lcDiamond + SUBSTR(laAllMod[lsModules],2)
  ELSE
    laAllMod[lsModules] = " " + SUBSTR(laAllMod[lsModules],2)
  ENDIF
ENDIF

_CUROBJ = _CUROBJ
SHOW GET lsModules

=lfStatus()

*!**************************************************************************
*!
*!      Function: lfvIns
*!
*!**************************************************************************
** Valid function for the push button < Install > OR < Reinstall > ...
*
FUNCTION lfvIns

*E300993,1 AMM Initialize some variables.
PRIVATE lcReqMod, lcModl, laTempArr
STORE SPACE(0) TO lcReqMod, lcModl
*E300993,1 AMM Create array to hold the reqired modules for each module.
DECLARE laReqMod[1,2]
SELECT '*'+cApp_Id,mReqModul ;
  FROM (gcSysHome+"SYDAPPL") ;
  WHERE IIF(EMPTY(laData(14)),.T.,!lparntmdl) ;
  INTO ARRAY lareqMod


*E300993,1 AMM start, Comment out
** If select some of the modules, install them. **
*FOR lnCount2 = 1 TO ALEN(laAllMod,1)
*  IF AT(lcDiamond,laAllMod[lnCount2]) = 0
*    LOOP
*  ENDIF
*E300993,1 AMM Create an array laTempArr to have the module level to sort on that 
*E300993,1 AMM level then install higher levels first.
DIMENSION laTempArr[ALEN(laAllMod,1),4]
FOR lnC=1 TO ALEN(laAllMod,1)
  *E300993,1 AMM previous data from the original module laAllMod
  laTempArr[lnC,2] = laAllMod[lnC]
  *E300993,1 AMM Module ID
  laTempArr[lnC,3] = SUBSTR(laAllMod[lnC],3,2)
  *E300993,1 AMM The number of laAllMod element (we will resort the laTempArr 
  *E300993,1 AMM and we want to preserve the element number of laAllMod)
  laTempArr[lnC,4] = lnC
ENDFOR
*E300993,1 AMM Get the module level in the modules tree
FOR lnC=1 TO ALEN(laAllMod,1)
  IF AT(lcDiamond,laAllMod[lnC]) = 0
    laTempArr[lnC,1] = 0
  ELSE
   laTempArr[lnC,1] = lfGetLevel(SUBSTR(laAllMod[lnC],3,2),lnC)
  ENDIF
ENDFOR
*E300993,1 AMM Sort the array
*=ASORT(laTempArr,1)

*E300993,1 Loop on all elements of laTempArr
FOR lnCnt = 1 TO ALEN(laTempArr,1)
  IF AT(lcDiamond,laTempArr[lnCnt,2]) = 0
    LOOP
  ENDIF
  lnCount2 = laTempArr[lnCnt,4]
  *E300993,1 AMM If there is modules required for that module.
  lnModPos = ASCAN(laReqMod,'*'+laTempArr[lnCnt,3]) + 1
  IF !EMPTY(laReqMod[lnModPos])
    lcReqMod = SPACE(0)
    lnPos1 = 1
    FOR lnC = 1 TO IIF(OCCURS('|',laReqMod[lnModPos])=0,1,OCCURS('|',laReqMod[lnModPos])+1)
      *E300993,1 AMM Get modules one by one.
      IF ATC('|',laReqMod[lnModPos],lnC) # 0
        lcModl = SUBSTR(laReqMod[lnModPos],lnPos1,ATC('|',laReqMod[lnModPos],lnC)-lnPos1)
      ELSE
        lcModl = SUBSTR(laReqMod[lnModPos],lnPos1)
      ENDIF
      lnPos1 = ATC('|',laReqMod[lnModPos],lnC)+1
      *E300993,1 AMM If the required module isn't installed , then fill that 
      *E300993,1 AMM variable and display message.
      IF !(lcModl $ lcOldMod) 
        lcReqMod = lcReqMod + IIF(EMPTY(lcReqMod),'',',')+lcModl
      ENDIF
    ENDFOR
    IF !EMPTY(lcReqMod)
      *-- Message "Module "+ laTempArr[lnCnt,3] +" requires the installation 
      *--         of " + lcReqMod + " module(s) prior to it, Cannot install."
      =gfModalGen("INM00320B00000","DIALOG",laTempArr[lnCnt,3]+"|"+lcReqMod)
      lcReqMod = SPACE(0)
      LOOP
    ENDIF
  ENDIF
  *E300993,1 AMM end
  
  
  lcCurModul = SUBSTR(laAllMod[lnCount2],3,2)
  lcCurLogNm = SUBSTR(ALLTRIM(LEFT(laAllMod[lnCount2],;
                      LEN(laAllMod[lnCount2])-3)),6,30)
  llSetReq   = IIF(RIGHT(laAllMod[lnCount2],1) = "T",.T.,.F.)

  ** Do you want to install files for module 'laAllMod[lnCount2]'? **
  ** <  Yes  > - <  No  > **
  IF gfModalGen("QRM00179B00006","DIALOG",lcCurLogNm) = 1
            
    ** Function to install files for current module. **
    IF lfInstall(lcCurModul)
     *B602147,1 Hesham (Start) if the user select yes for installing
     *B602147,1 ic module files then
     *B602147,1 copy file icistru without checking if it is exist in the
     *B602147,1 system files directory so if it does not exit an error
     *B602147,1 occus and the module will not be installed

     *E301469,1 AMH install current module for History company[Start]
     IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
       lnActAlias =  SELECT(0)
       SELECT SYCCOMP
       =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
       lcActPath = laData[11]
       laData[11] = ALLTRIM(SYCCOMP.ccom_ddir)
       =lfInstall(lcCurModul)
       laData[11] = lcActPath
       SELECT SYCCOMP
       =SEEK(ALLTRIM(laData[1]))
       SELECT (lnActAlias)
     ENDIF 	
     *E301469,1 AMH [End]

     IF lcCurModul $ 'IC,PS'
        lcItemFil = gcSysHome + "ICISTRU"      
        lcItemAlias = gfTempName()
        USE (ALLTRIM(laData[11])+"ICISTRU.DBF") AGAIN ALIAS &lcItemAlias IN 0
        SELECT (lcItemAlias)
        DELETE FOR citemrecty <> 'U'
        APPEND FROM (lcItemFil)
        USE IN (lcItemAlias)
      ENDIF
     *B602147,1 Hesham (End)      
      IF AT(lcCurModul,lcOldMod) = 0
        lcOldMod   = lcOldMod + IIF(EMPTY(lcOldMod),'','|')+lcCurModul
      ENDIF
      laData[15] = laData[15] + IIF(EMPTY(laData[15]),'','|')+lcCurModul
*      IF llSavMod
        SELECT sycComp
        REPLACE mComp_mdl WITH laData[15]
*      ENDIF

        *E301469,1 AMH Update mComp_mdl field for History company[Start]
        IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
          =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
          REPLACE mComp_mdl WITH laData[15]
          =SEEK(ALLTRIM(laData[1]))
        ENDIF 	
        *E301469,1 AMH [End]

      laAllMod[lnCount2] = " "+SUBSTR(laAllMod[lnCount2],2,36)+;
                           "Installed    "+RIGHT(laAllMod[lnCount2],1)
            
      IF llSetReq
        ** Setup is required for module 'laAllMod[lnCount2]'. **
        ** Do you want to setup the module now? **
        ** <  Yes  > - <  No  > **
        IF gfModalGen("QRM00181B00006","DIALOG",lcCurLogNm) = 1
          
          ** If fiscal years not added successfully. **
          IF !llSavFis
            ** You cannot setup module {laAllMod[lnCount2]} **
            ** till you add fiscal years for this company **
            ** <  Ok  > **
            =gfModalGen("TRM00175B00000","ALERT",lcCurLogNm)
            LOOP
          ENDIF
                  
          ** If account code strucure not added successfully. **
          IF !llSavAcc
            ** You cannot setup module {laAllMod[lnCount2]} **
            ** till you add account code strucure for this **
            ** company...
            ** <  Ok  > **
            =gfModalGen("TRM00176B00000","ALERT",lcCurLogNm)
            LOOP
          ENDIF
                  
          ** Setup for cuurent module. **

          =lfSetModul(lcCurModul)
          *E301469,1 AMH copy apsetup & glsetup for History company[Start]
          IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
            lnAlias = SELECT(0)
            SELECT SYCCOMP
            =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
            lcSorDir = ALLTRIM(laData[11])
            lcTrgDir = ALLTRIM(SYCCOMP.ccom_ddir)
            =SEEK(ALLTRIM(laData[1]))
            SELECT (lnAlias)
            IF lcCurModul = 'AP'
              COPY FILE lcSorDir+'apsetup.dbf' TO lcTrgDir+'apsetup.dbf'
            ENDIF
            IF lcCurModul = 'GL'
              COPY FILE lcSorDir+'glsetup.dbf' TO lcTrgDir+'glsetup.dbf'
              COPY FILE lcSorDir+'glsetup.fpt' TO lcTrgDir+'glsetup.fpt'
            ENDIF
          ENDIF
          *E301469,1 AMH [End]
                
        ENDIF
      ELSE
        SELECT SYCCOMP
        *B601905,1 Hesham El-Sheltawi (Start)
        *B601905,1 Fix the saving for the memo field that save the installed modules
        *B601905,1 for the company
        *REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul        
        IF !(lcCurModul $ mModlSet)
          REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul
        ENDIF  

        *E301469,1 AMH Update mModlSet field for History company[Start]
        IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
          =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
          IF !(lcCurModul $ mModlSet)
            REPLACE mModlSet  WITH mModlSet+IIF(EMPTY(mModlSet),'',',')+lcCurModul
          ENDIF  
          =SEEK(ALLTRIM(laData[1]))
        ENDIF 	
        *E301469,1 AMH [End]

        *B601905,1 Hesham El-Sheltawi (End)        
        *B601801,1 Hesham El-Sheltawi (Start)
        *B601801,1 Make the global variable that hold the installed modules for the 
        *B601801,1 company updated if the modified company is the active company
        IF laData[1] = gcAct_Comp
          gcCmpModules = ALLTRIM(mModlSet)
        ENDIF
        *B601801,1 Hesham El-Sheltawi (End)  
      ENDIF
    ELSE
      ** Installing files for module [laAllMod[lnCount2] **
      ** not done successfully...
      ** <  Ok  > **
      =gfModalGen("TRM00180B00000","DIALOG",lcCurLogNm)
      laAllMod[lnCount2] = " " + SUBSTR(laAllMod[lnCount2],2)
    ENDIF
  ENDIF
ENDFOR

_CUROBJ = OBJNUM(lsModules)
SHOW GET lsModules

*!**************************************************************************
*!
*!      Function: lfvUnIns
*!
*!**************************************************************************
** Valid function for the push button < Uninstall > ...
*
FUNCTION lfvUnIns

** Are you sure you want to uninstall **
** this module ?  You will  loose all **
** files related to this module...
** <  Yes  > - <  No  > **
IF gfModalGen("QRM00184B00006","ALERT") = 2
  RETURN
ENDIF

SELECT SYUSTATC
GO TOP
LOCATE FOR ALLTRIM(cComp_id) = ALLTRIM(laData[1])
IF FOUND()
  ** There is a user using this company now, **
  ** You cannot uninstall any modul for this **
  ** company now...
  ** <  Ok  > **
  =gfModalGen("INM00216B00000","DIALOG",laData[1])
  SELECT sycComp
  RETURN
ENDIF

SELECT SYCCOMP

** Call the function of Uninstall module. **
=lfUnInstal(SUBSTR(laAllMod[lsModules],3,2))

SHOW GET pbUnIns DISABLE
SHOW GET pbSet   DISABLE

*!**************************************************************************
*!
*!      Function: lfvSet
*!
*!**************************************************************************
** Valid function of the push button < Setup > ...
*
FUNCTION lfvSet
** Check if the fiscal year data was entered or not. **
IF !llSavFis
  ** You cannot setup module {laAllMod[lsModules]} **
  ** till you add fiscal years for this company **
  ** <  Ok  > **
*B601515,1 M.H Begin.
*  =gfModalGen("TRM00175B00000","ALERT",;
          SUBSTR(ALLTRIM(LEFT(laAllMod[lsModules],;
          LEN(laAllMod[lsModules])-3)),6,30))

  =gfModalGen("TRM00175B00000","ALERT",;
          ALLTRIM(SUBSTR(ALLTRIM(LEFT(laAllMod[lsModules],;
          LEN(laAllMod[lsModules])-3)),4,30)))
*B601515,1 M.H End.
  RETURN
ENDIF

** Check if the account code strucure data was entered or not. **
IF !llSavAcc
  ** You cannot setup module {laAllMod[lsModules]} **
  ** till you add account code strucure for this **
  ** company...
  ** <  Ok  > **
*B601515,1 M.H Begin.
*  =gfModalGen("TRM00176B00000","ALERT",;
          SUBSTR(ALLTRIM(LEFT(laAllMod[lsModules],;
          LEN(laAllMod[lsModules])-3)),6,30))
  =gfModalGen("TRM00176B00000","ALERT",;
          ALLTRIM(SUBSTR(ALLTRIM(LEFT(laAllMod[lsModules],;
          LEN(laAllMod[lsModules])-3)),4,30)))
*B601515,1 M.H End.
  RETURN
ENDIF

** Call the setup function. **
lcCurLogNm = SUBSTR(ALLTRIM(LEFT(laAllMod[lsModules],;
                    LEN(laAllMod[lsModules])-3)),6,30)


=lfSetModul(SUBSTR(laAllMod[lsModules],3,2))

*!**************************************************************************
*!
*!      Function: lfInstall
*!
*!**************************************************************************
** Installtion function called from :_
**       Edit mode : _ lfvIns   { valid function of the push button 
**                              < Install > } in SMSELMD screen...
*
FUNCTION lfInstall
PARAMETERS lcMod2Ins
lcUpGrdLvl = IIF(lcMod2Ins='SY','S','A')
** Get all the files related to this module. **
SELECT SYDFILES
DECLARE laMdlFiles[1,3]
laMdlFiles = " "
_TALLY     = 0
*B601803,1 Select all files with module = lcMod2Ins in mFile_App
*SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
       FROM (gcSysHome+"SYDFILES") ;
       WHERE lcMod2Ins IN (mfile_app) .AND. ;
             LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
       INTO ARRAY laMdlFiles
SELECT CFILE_NAM,CFILE_TTL,MFILE_APP ;
       FROM (gcSysHome+"SYDFILES") ;
       WHERE lcMod2Ins $ (mfile_app) .AND. ;
             LEFT(CFILE_NAM,2) <> "SY" AND CUPGRDLVL=lcUpGrdLvl;
       INTO ARRAY laMdlFiles       
*B601803,1 end
lnRecords = _TALLY

IF _TALLY = 0
  ** There is no files to be installed for this module. **
  ** <  Ok  > **
  =gfModalGen("TRM00186B00000","DIALOG")
  RETURN .F.
ENDIF

DECLARE laFil2Upd[1]
laFil2Upd = " "

*B602350,1 Set llYes2All to .T. to update all file structures without a 
*B602350,1 confirmation message. This variable is used by lfVer_Upd()
llYes2All = .T.
*B602350,1 end

** Loop in the files related to the module **
** to be installed...
FOR lnCount5 = 1 TO ALEN(laMdlFiles,1)
  lcFileNam  = UPPER(ALLTRIM(laMdlFiles[lnCount5,1]))
  lcPathFile = ALLTRIM(laData[11])+lcFileNam
  =gfThermo(lnRecords,lnCount5,"Creating: ",laMdlFiles[lnCount5,2])
  *B602147,1 Hesham (Start)
  *B602147,1 call global function to create or update file structure
  *B602147,1 insted of creating the file directly
  lcTargDir = ALLTRIM(laData[11])
  lnSelected = lnCount5
  =lfver_upd(lcFileNam,.T.)
  LOOP
  *B602147,1 Hesham (End)
  ** Select the file structure in Array **
  DECLARE laFileStrc[1,5]
  laFileStrc = " "
*B601701,1 Hesham El-Sheltawi (Start)  
*B601701,1 colect the struct. of the file name 
*B601701,1 padded right with 8 char so it wont
*B601701,1 get wrong structure in case of the file name
*B601701,1 length is less that 8 char
*  SELECT sydflfld.cfld_name,sydfield.cdata_typ, ;
         sydfield.nfld_wdth,sydfield.nfld_dec, ;
         sydflfld.nfld_pos ;
   FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield") ;
         ORDER BY sydflfld.nfld_pos ;
         GROUP BY sydField.cFld_Name ;
   WHERE UPPER(sydflfld.cfile_nam) = lcFileNam .AND. ;
         sydfield.cfld_name = sydflfld.cfld_name ;
         INTO ARRAY laFileStrc
      
  SELECT sydflfld.cfld_name,sydfield.cdata_typ, ;
         sydfield.nfld_wdth,sydfield.nfld_dec, ;
         sydflfld.nfld_pos ;
   FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield") ;
         ORDER BY sydflfld.nfld_pos ;
         GROUP BY sydField.cFld_Name ;
   WHERE UPPER(sydflfld.cfile_nam) = PADR(lcFileNam,8) .AND. ;
         sydfield.cfld_name = sydflfld.cfld_name ;
         INTO ARRAY laFileStrc
*B601701,1 Hesham El-Sheltawi (End)  
  =gfAdel(@laFileStrc,5,2)  && An additional colum holding the position 
                            && of each field has to be removed from the
                            && Array befor verification or building.
  
  ** If function called from add mode override the files. **
  ** If function called from edit mode check first. **
  IF llSavMod 
    ** Overwrite the file or create. **
    =lfCreatFil()
  ELSE
    IF !FILE(lcPathFile+".DBF")
      ** Create the file. **
      =lfCreatFil()
    ELSE
      IF AT(",",laMdlFiles[lnCount5,3]) = 0
        ** If not involved in any other module. overwrite it. **
        =lfCreatFil()
      ELSE
        ** If the file is involved in more than one modul **
        ** update its strucure not to overwrite it...
        =lfCoparStr()
      ENDIF
    ENDIF
  ENDIF
ENDFOR
*E301065,1 AMM Call the function, to add records of the form codes
=lfGetFC(lcMod2Ins,ALLTRIM(laData[11]),'A')
*E301065,1 AMM end

*E30703,1 Hesham (Start)
*E30703,1 if intalling ic module then copy file ICISTRU item code struc.
*E30703,1 to the company data dir. with the predefined code struct.
IF lcMod2Ins = "IC"
  lcItemFil = gcSysHome + "ICISTRU"
  *B602147,1 Hesham (Start)
  *B602147,1 copy file icistru without checking if it is exist in the
  *B602147,1 system files directory so if it does not exit an error
  *B602147,1 occus and the module will not be installed
*  IF FILE(lcItemFil+".DBF") 
*    COPY FILE (lcItemFil+".DBF") TO (ALLTRIM(laData[11])+"ICISTRU.DBF")
*  ENDIF
*  IF FILE(lcItemFil+".CDX") 
*    COPY FILE (lcItemFil+".CDX") TO (ALLTRIM(laData[11])+"ICISTRU.CDX")
*  ENDIF
*  IF FILE(lcItemFil+".FPT")
*    COPY FILE (lcItemFil+".FPT") TO (ALLTRIM(laData[11])+"ICISTRU.FPT")
*  ENDIF
*    COPY FILE (lcItemFil+".DBF") TO (ALLTRIM(laData[11])+"ICISTRU.DBF")
*    COPY FILE (lcItemFil+".CDX") TO (ALLTRIM(laData[11])+"ICISTRU.CDX")
    *COPY FILE (lcItemFil+".FPT") TO (ALLTRIM(laData[11])+"ICISTRU.FPT")
  *B602147,1 Hesham (Start)  
ENDIF
*E30703,1 Hesham (End)

** If general ledger modul, copy this files **
** to the same path from sysfiles dir. :_
** Cash flow items.  "GLCFITEM"
** Ratio groups.     "GLRACOD"
** Source journals.  "GLSUBJOR"
IF lcMod2Ins = "GL"
  lcItemFil = gcSysHome + "GLCFITEM"
  IF FILE(lcItemFil+".DBF") AND FILE(lcItemFil+".CDX") AND FILE(lcItemFil+".FPT")
    ** Copy Cash flow items. **
    COPY FILE (lcItemFil+".DBF") TO (ALLTRIM(laData[11])+"GLCFITEM.DBF")
    COPY FILE (lcItemFil+".CDX") TO (ALLTRIM(laData[11])+"GLCFITEM.CDX")
    COPY FILE (lcItemFil+".FPT") TO (ALLTRIM(laData[11])+"GLCFITEM.FPT")
  ENDIF
  lcRacFil = gcSysHome + "GLRACOD"
  IF FILE(lcRacFil+".DBF") AND FILE(lcRacFil+".CDX") AND FILE(lcRacFil+".FPT")
    ** Copy Ratio groups. **
    COPY FILE (lcRacFil+".DBF") TO (ALLTRIM(laData[11])+"GLRACOD.DBF")
    COPY FILE (lcRacFil+".CDX") TO (ALLTRIM(laData[11])+"GLRACOD.CDX")
    COPY FILE (lcRacFil+".FPT") TO (ALLTRIM(laData[11])+"GLRACOD.FPT")
  ENDIF
  lcSubFil = gcSysHome + "GLSUBJOR"
  IF FILE(lcSubFil+".DBF") AND FILE(lcSubFil+".CDX") AND FILE(lcSubFil+".FPT")
    ** Copy Source journals. **
    COPY FILE (lcSubFil+".DBF") TO (ALLTRIM(laData[11])+"GLSUBJOR.DBF")
    COPY FILE (lcSubFil+".CDX") TO (ALLTRIM(laData[11])+"GLSUBJOR.CDX")
    COPY FILE (lcSubFil+".FPT") TO (ALLTRIM(laData[11])+"GLSUBJOR.FPT")
  ENDIF
ENDIF

IF !EMPTY(laFil2Upd[1])
  *E300683,4 Call *.SPR from screens directory
  * DO SMDSPFL.SPR 
  DO (gcScrDir + gcWinAppl + '\SMDSPFL.SPR')
  *E300683,4 end  
ENDIF

*!**************************************************************************
*!
*!      Function: lfCreatFil
*!
*!**************************************************************************
*
FUNCTION lfCreatFil


SELECT SYDINDEX
DECLARE laFileCDX[1,4]
laFileCDX = " "
** Select the file index in Array **
*B601701,1 Hesham El-Sheltawi (Start)  
*B601701,1 colect the indexes of the file name 
*B601701,1 padded right with 8 char so it wont
*B601701,1 get wrong structure in case of the file name
*B601701,1 length is less that 8 char
*SELECT sydindex.cindx_exp,sydindex.cfile_tag, ;
       sydindex.lascend,sydindex.lunique ;
       FROM (gcSyshome+"sydindex") ;
       WHERE UPPER(sydindex.cfile_nam) = lcFileNam ;
       INTO ARRAY laFileCDX

SELECT sydindex.cindx_exp,sydindex.cfile_tag, ;
       sydindex.lascend,sydindex.lunique ;
       FROM (gcSyshome+"sydindex") ;
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8) ;
       INTO ARRAY laFileCDX
*B601701,1 Hesham El-Sheltawi (End)  
CREATE DBF (lcPathFile) FROM ARRAY laFileStrc

IF !EMPTY(laFileCDX[1])
  FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
    lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
    lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
    INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2]; 
             ADDITIVE &lcAscend &lcUnique
  ENDFOR            
ENDIF
USE

*!**************************************************************************
*!
*!      Function: lfCoparStr
*!
*!**************************************************************************
*
FUNCTION lfCoparStr

llOpen = .F.
IF !USED(lcFileNam)
  SELECT 0
  USE &lcPathFile
  llOpen = .T.
ELSE
  SELECT (lcFileNam)
ENDIF
** Actual file structure. **
DECLARE laActStrc[1,4]
laActStrc = " "
=AFIELDS('laActStrc')
  
** If opened just now, close this file. **
IF llOpen
  SELECT (lcFileNam)
  USE
ENDIF

** Compare the actual strucure & the dictionary strucure. **
FOR lnCont9 = 1 TO ALEN(laFileStrc,1)
  IF TYPE('laActStrc[lnCont9,1]') <> 'U'
    IF laFileStrc[lnCont9,1] <> laActStrc[lnCont9,1]
      IF !EMPTY(laFil2Upd[1])
        DECLARE laFil2Upd[ALEN(laFil2Upd,1)+1]
      ENDIF
      laFil2Upd[ALEN(laFil2Upd,1)] = lcFileNam + "  " + ;
                                     laMdlFiles[lnCount5,2]
      EXIT
    ENDIF
  ELSE
    IF !EMPTY(laFil2Upd[1])
      DECLARE laFil2Upd[ALEN(laFil2Upd,1)+1]
    ENDIF
    laFil2Upd[ALEN(laFil2Upd,1)] = lcFileNam + "  " + ;
                                   laMdlFiles[lnCount5,2]
    EXIT
  ENDIF
ENDFOR

*!**************************************************************************
*!
*!      Function: lfSetModul
*!
*!**************************************************************************
** Setup function called from :_
**       Edit mode : _ lfvSet   { valid function of the push button 
**                              < Setup > } in SMSELMD screen...
*
FUNCTION lfSetModul
PARAMETERS lcMod2Set,llGlobalSet

lcOldActCm = gcAct_comp
lcOldDataD = gcDataDir
lcOldActMd = gcAct_Appl

gcAct_Appl = lcMod2Set
gcAct_comp = laData[1]


*E301098,1 Use gfGetDataDir
*gcDataDir  = ALLTRIM(laData[11])
gcDataDir  = gfGetDataDir(ALLTRIM(laData[11]))
*E301098,1 end

SELECT SYCCOMP
lnRecCmp = RECNO()

IF EMPTY(laData[14])
  lcInsMdl  = laData[15]
ELSE
  IF SEEK(laData[14],"SYCCOMP")
    lcInsMdl  = syccomp.mcomp_mdl
  ELSE
    lcInsMdl  = laData[15]
  ENDIF
ENDIF

SELECT SYDOBJCT
SET ORDER TO TAG CAPP_ID

** Check if the program of module setup is **
** exist in the module objects or not...
*E300633,1 check if the module have it's own setup program then run
*E300633,1 it else run the global module setup program
llGlobalSet = !SEEK(lcMod2Set+lcMod2Set+'SETUP','SYDOBJCT')
*IF SEEK(lcMod2Set+lcMod2Set+'SETUP','SYDOBJCT') OR llGlobalSet
*E300633,1 Hesham El-Sheltawi (End)
  SELECT sycComp
      ** Save the current environment in the static file. **
      lcCurrWind = gcBaseWind
      =gfStatic()
      lcWindow   = UPPER(lcBaseWind)
      glFirsTime = .T.
      IF !llGlobalSet
        gcBaseWind ='AWD' + lcMod2Set + 'SETUP'
      
        ** Call the module setup program  to add **
        ** the account code struc. for the added **
        ** comapny...
        lcDataDir = ALLTRIM(laData[11])
      
        *E301098,1 Use gfGetDataDir
        *lcDataDir = ALLTRIM(laData[11])
        lcDataDir = gfGetDataDir(ALLTRIM(laData[11]))
        *E301098,1 end
      
        lcAppName  = gcAppHome +lcMod2Set + '.APP'
        *E300683,4 Pass a parameter to AP.PRG or GL.APP
        *DO (lcAppName) WITH 'lcMod2Set+"SETUP" WITH ;
                             .T.,"&laData[1]","&lcDataDir","&lcInsMdl"'
        DO (lcAppName) WITH '&lcMod2Set.SETUP',;
                             '.T.,"&laData[1]","&lcDataDir","&lcInsMdl"'                             
        *E300683,4 end
                     
      *E300633,1 Hesham El-Sheltawi (Start)
      *E300633,1 if the module does not have it's own setup program
      *E300633,1 then run the global module setup program
      ELSE
        lcAppName  = gcAppHome +lcMod2Set + '.APP'
        *E301315,1 Hesham (Start)
        *E301315,1 IF the module application file does not exist
        *E301315,1 then run the SM.app
        IF !FILE(lcAppName)
          *lcAppName = gcAppHome + 'SM.APP'
          DO SYCMSET WITH lcMod2Set,gcdataDir,laData[1]
        ELSE
          DO (lcAppName) WITH 'SYCMSET WITH "&lcMod2Set","&gcdataDir","&laData[1]"'                            
        ENDIF

        *DO (lcAppName) WITH 'SYCMSET WITH "&lcMod2Set","&gcdataDir","&laData[1]"'        
        *E301315,1 Hesham (End)

        
      *E300633,1 Hesham El-Sheltawi (End)  
      ENDIF                       
     
      lcSavExact = SET('EXACT')
      SET EXACT ON

      IF SEEK ('WIN'+lcWindow+UPPER(gcUser_Id)+gcStation,'syustatc')   
        ** Restore data for the company program from static file. **
        RESTORE FROM MEMO syustatc.mObj_Data  ADDITIVE
      ENDIF  
      SET EXACT &lcSavExact
      gcBaseWind = lcCurrWind
*ELSE
  ** The Setup program for Module ð not found **
  ** <  Ok  > **
*  =gfModalGen("INM00090B00000","ALERT",lcCurLogNm)
*ENDIF  

*E301077,78 Hesham (Start)
=gfOpenFile(gcSysHome+"SYCCOMP",'CCOMP_ID')         
*E301077,78 Hesham (End)

glQuitting = .F.

SELECT sycComp
IF lnRecCmp > 0 .AND. lnRecCmp<= RECCOUNT()
  GO lnRecCmp
ENDIF
*E300627,1 Hesham El-Sheltawi  (Start)
*E300627,1 add the module that was installed for the comp. installed
*E300627,1 modules
llModSetUse = .T.
IF !llGlobalSet
  llModSetUse = USED(lcMod2Set+'SETUP')
  IF !llModSetUse
    USE (gcDataDir+lcMod2Set+'SETUP') IN 0
  ENDIF  
  SELECT (lcMod2Set+'SETUP')
  llModuleIns = lSetDon
ELSE
  lcTmpSetAl = gfTempName()  
  SELECT 0
  USE (gcDataDir+'SETUPS') AGAIN ALIAS &lcTmpSetAl
  SET ORDER TO TAG NVARPOS
  llModuleIns = SEEK(lcMod2Set)
  USE IN (lcTmpSetAl)
ENDIF  
SELECT SYCCOMP

*B601905,1 Hesham El-Sheltawi (Start)
*B601905,1 Fix the saving for the memo field that save the installed modules
*B601905,1 for the company
*REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
IF !(lcMod2Set $ mModlSet)
  REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
ENDIF  
*E301469,1 AMH Update mModlSet field for History company[Start]
IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
  IF !(lcMod2Set $ mModlSet)
    REPLACE mModlSet  WITH mModlSet+IIF(llModuleIns,IIF(EMPTY(mModlSet),'',',')+lcMod2Set,'')
  ENDIF  
  =SEEK(ALLTRIM(laData[1]))
ENDIF 	
*E301469,1 AMH [End]
*B601905,1 Hesham El-Sheltawi (End)
*B601801,1 Hesham El-Sheltawi (Start)
*B601801,1 Make the global variable that hold the installed modules for the 
*B601801,1 company updated if the modified company is the active company
IF laData[1] = gcAct_Comp
  gcCmpModules = ALLTRIM(mModlSet)
ENDIF  
*B601801,1 Hesham El-Sheltawi (End)
IF !llModSetUse
  USE IN (lcMod2Set+'SETUP')
ENDIF  
*E300627,1 Hesham El-Sheltawi  (End)
gcAct_Appl = lcOldActMd
gcAct_Comp = lcOldActCm
gcDataDir  = lcOldDataD

*!**************************************************************************
*!
*!      Function: lfUnInstal
*!
*!**************************************************************************
** Setup function called from :_
**       Edit mode : _ lfvSet   { valid function of the push button 
**                              < Setup > } in SMSELMD screen...
** Using parameteres................:_
**       Module ID.
*
FUNCTION lfUnInstal
PARAMETERS lcMod2UIns

SELECT SYCCOMP
laData[15] = "|" + laData[15]
laData[15] = STRTRAN(laData[15],"|"+lcMod2UIns,"")
laData[15] = SUBSTR(laData[15],2)
*B603744,1 (Begin) In case of no active company selected, gcDataDir is empty generating the bug.
lcOldDataD = gcDataDir
gcDataDir  = gfGetDataDir(ALLTRIM(laData[11]))
*B603744,1 (End)


REPLACE mComp_mdl WITH laData[15] 
*E301469,1 AMH Update mComp_mdl field for History company[Start]
IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
  REPLACE mComp_mdl WITH laData[15]
  =SEEK(ALLTRIM(laData[1]))
ENDIF 	
*E301469,1 AMH [End]
*E300627,1 Hesham El-Sheltawi  (Start)
*E300627,1 remove the module that was uninstalled from the comp. installed
*E300627,1 modules
SELECT SYDOBJCT
SET ORDER TO TAG CAPP_ID
llGlobalSet = !SEEK(lcMod2UIns+lcMod2UIns+'SETUP','SYDOBJCT')
IF llGlobalSet 
  *E301469,1 AMH Remove module setups for History company[Start]
  lnLoop = IIF(!GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]));
               ,2,1)
  FOR lnI = 1 TO lnLoop
    IF lnI = 2
      lcOldDir = gcDataDir
      SELECT SYCCOMP
      =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
      gcDataDir = ALLTRIM(SYCCOMP.ccom_ddir)
      SELECT SYDOBJCT
    ENDIF
    lcTmpSetAl = gfTempName()  
    SELECT 0
    USE (gcDataDir+'SETUPS') AGAIN ALIAS &lcTmpSetAl
    SET ORDER TO TAG NVARPOS
    IF SEEK(lcMod2UIns)
      DELETE WHILE CAPP_ID+STR(NVARPOS) = lcMod2UIns
    ENDIF
    USE IN (lcTmpSetAl)
    IF lnI = 2
      gcDataDir = lcOldDir
    ENDIF
  ENDFOR
  *E301469,1 AMH [End]
ENDIF
SELECT SYCCOMP
*B601905,1 Hesham El-Sheltawi (Start)
*B601905,1 Fix the saving for the memo field that save the installed modules
*B601905,1 for the company
*REPLACE mModlSet  WITH STRTRAN(STRTRAN(mModlSet,','+lcMod2UIns),lcMod2UIns)
IF lcMod2UIns $ mModlSet
  REPLACE mModlSet  WITH STRTRAN(mModlSet,','+lcMod2UIns)
  REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns+',')
  REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns)
ENDIF  
*E301469,1 AMH Update mModlSet field for History company[Start]
IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
  =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
  IF lcMod2UIns $ mModlSet
    REPLACE mModlSet  WITH STRTRAN(mModlSet,','+lcMod2UIns)
    REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns+',')
    REPLACE mModlSet  WITH STRTRAN(mModlSet,lcMod2UIns)
  ENDIF  
  =SEEK(ALLTRIM(laData[1]))
ENDIF 	
*E301469,1 AMH [End]
*B601905,1 Hesham El-Sheltawi (End)
*E300627,1 (End)
lcOldMod = "|" + lcOldMod
lcOldMod = STRTRAN(lcOldMod,"|"+lcMod2UIns,"")
lcOldMod = SUBSTR(lcOldMod,2)

laAllMod[lsModules] = SUBSTR(laAllMod[lsModules],1,37)+;
                         "             "+RIGHT(laAllMod[lsModules],1)

** Do you want to delete files for module ð. **
** <  Delete  > - <  Cancel  > **
IF gfModalGen("QRM00217B00002","ALERT",lcMod2UIns) = 1
  =lfDel_Mdl(lcMod2UIns)
ENDIF
*E301065,1 AMM Call the function to remove records from the form codes files
=lfGetFC(lcMod2UIns,ALLTRIM(laData[11]),'D')
*E301065,1 AMM end
*B603744,1 (Begin) Restore the old data dir
gcDataDir  = lcOldDataD
*B603744,1 (End)
SHOW GET lsModules

*!**************************************************************************
*!
*!      Function: lfDel_Fil
*!
*!**************************************************************************
*
FUNCTION lfDel_Fil

** Get the files to be deleted. **
SELECT SYDFILES
SET ORDER TO TAG CFILE_NAM

lnRecords = 0

** Calculat No. of files to be deleted. **

*E300611,1 Change this line to delete the system files that 
*          was removed to the data dir. [Begin]
*COUNT FOR LEFT(CFILE_NAM,2) <> "SY" .AND. ;
*          !EMPTY(MFILE_APP) .AND. ;
*          !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM");
*          TO lnRecords

COUNT FOR (LEFT(CFILE_NAM,2) <> "SY" .AND. ;
          !EMPTY(MFILE_APP) .AND. ;
          !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM"));
          .OR. ('SY' $ MFILE_APP .AND. LEFT(CFILE_NAM,2) <> 'SY');
          TO lnRecords

*E300611,1 Change this line [Begin]

IF lnRecords = 0
  ** There is no files to be deleted. **
  ** <  Ok  > **
  =gfModalGen("TRM00197B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Trap any error apear in the next loop. **
lcOnError  = ON('ERROR')
ON ERROR DO lpErrTrap WITH ERROR()

lcSavDefa = SET("DEFAULT")
lnCRec    = 1

** Loop to delete company files

*E300611,1 Change this line to delete the system files that 
*          was removed to the data dir. [Begin]
*SCAN FOR LEFT(CFILE_NAM,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
*         !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM") 

SCAN FOR (LEFT(CFILE_NAM,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
         !(MFILE_APP $ "SM,SY,SM$,SY$,SM$,SY,SM,SY$,SM")); 
         .OR. ('SY' $ MFILE_APP .AND. LEFT(CFILE_NAM,2) <> 'SY')

*E300611,1 Change this line [End]

  lcFilNam  = ALLTRIM(sydFiles.cfile_nam)         && Without ext
  lcFilName = ALLTRIM(sydFiles.cfile_nam)+".DBF"  && With ext
  
  lcDelPath = ALLTRIM(laData[11])
  lcPathFile= lcDelPath+lcFilName          && With ext and path
  
  =gfThermo(lnRecords,lnCRec,"Deleting Company files:",lcPathFile)
  
  IF FILE(lcPathFile) 
    SELECT 0 
    USE (lcPathFile) EXCLUSIVE 
    USE
  
    IF llError
      ** Error Deleting Do you want to continue ? **
      ** < Yes > -<  No  > **
      IF gfModalGen("QRM00034B00006","ALERT",+" "+lcPathFile) = 1
        llError=.F.
      ELSE
        EXIT
      ENDIF   
    ELSE
      ** No one is using this file so delete it
      ERASE (lcDelPath+lcFilNam+".DBF")
      ERASE (lcDelPath+lcFilNam+".CDX")
      ERASE (lcDelPath+lcFilNam+".FPT")
    ENDIF 
  ENDIF
  lnCRec = lnCRec + 1
  SELECT SYDFILES
ENDSCAN  

SET DEFAULT TO &lcSavDefa  
ON ERROR &lcOnError

SELECT sycComp

*!**************************************************************************
*!
*!      Function: lfDel_Mdl
*!
*!**************************************************************************
*
FUNCTION lfDel_Mdl
PARAMETERS lcModul

** Get the files to be deleted. **
SELECT SYDFILES
SET ORDER TO TAG CFILE_NAM

lnRecords = 0

** Calculat No. of files to be deleted. **
COUNT FOR LEFT(CFILE_NAM,2) <> "SY" .AND. ;
          !EMPTY(MFILE_APP) .AND. ;
          (lcModul $ MFILE_APP) .AND. ;
          !("," $ MFILE_APP) ;
          TO lnRecords

IF lnRecords = 0
  ** There is no files to be deleted. **
  ** <  Ok  > **
  =gfModalGen("TRM00197B00000","DIALOG")
  SELECT sycComp
  RETURN
ENDIF

** Trap any error apear in the next loop. **
lcOnError  = ON('ERROR')
ON ERROR DO lpErrTrap WITH ERROR()

lcSavDefa = SET("DEFAULT")
lnCRec    = 1

** Loop to delete company files
SCAN FOR LEFT(CFILE_NAM,2) <> "SY" .AND. !EMPTY(MFILE_APP) .AND.;
         (lcModul $ MFILE_APP) .AND. !("," $ MFILE_APP)

  lcFilNam  = ALLTRIM(sydFiles.cfile_nam)         && Without ext
  lcFilName = ALLTRIM(sydFiles.cfile_nam)+".DBF"  && With ext
  
  lcDelPath = ALLTRIM(laData[11])
  lcPathFile= lcDelPath+lcFilName          && With ext and path
  
  =gfThermo(lnRecords,lnCRec,"Deleting Module "+lcModul+" files:",lcPathFile)
  
  IF FILE(lcPathFile) 
    SELECT 0 
    USE (lcPathFile) EXCLUSIVE 
    USE
  
    IF llError
      ** Error Deleting Do you want to continue ? **
      ** < Yes > -<  No  > **
      IF gfModalGen("QRM00034B00006","ALERT",+" "+lcPathFile) = 1
        llError=.F.
      ELSE
        EXIT
      ENDIF   
    ELSE
      ** No one is using this file so delete it
      ERASE (lcDelPath+lcFilNam+".DBF")
      ERASE (lcDelPath+lcFilNam+".CDX")
      ERASE (lcDelPath+lcFilNam+".FPT")
    ENDIF 
  ENDIF
  lnCRec = lnCRec + 1
  SELECT SYDFILES
ENDSCAN  

SET DEFAULT TO &lcSavDefa  
ON ERROR &lcOnError

SELECT sycComp

*!**************************************************************************
*!
*!      Procedure : lpErrTrap
*!
*!**************************************************************************
*
PROCEDURE lpErrTrap
PARAMETERS ERRNO

IF ERRNO = 108 .OR. ERRNO = 3
  llError = .T.  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfwCurrency

lcOldCurr=laData[17]

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfvCurrency

IF (!EMPTY(laData[17]) AND !SEEK(laData[17],'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laTemp")
  laData[17]=laTemp[1]
  SELECT (lcSelect)
  SHOW GET laData[17]
ENDIF

IF EMPTY(laData[17])
  laData[17] = lcOldCurr
ENDIF
llBrowse = .F.
=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfGetCur
*!
*!**************************************************************************
*
FUNCTION lfGetCur

laData[17] = LOOKUP(SYCINT.cCurrCode,laData[16],sycint.ccont_code,'CCONTCODE')
SHOW GET laData[17]
=lfRefresh(WOUTPUT())

*!**************************************************************************
*!
*!      Function: lfvData_18
*!
*!**************************************************************************
*
FUNCTION lfvData_18

IF laData[18]
  lcMultCurr = 'ENABLE'
  laData[19] = .T.
  laData[20] = .T.
  laData[21] = .F.
  laData[22] = 30
ELSE
  lcMultCurr = 'DISABLE'
  laData[19] = .F.
  laData[20] = .F.
  laData[21] = .F.
  laData[22] = 0
ENDIF
=lfShowCurr()

*!**************************************************************************
*!
*!      Function: lfShowCurr
*!
*!**************************************************************************
*
FUNCTION lfShowCurr
*SICO START
*SHOW GET laData[18] &lcMultCCnt
*SHOW GET laData[19] &lcMultCurr
*SHOW GET laData[20] &lcMultCurr
*SHOW GET laData[21] &lcMultCurr
*SHOW GET laData[22] &lcMultCurr
*SICO END


*!**************************************************************************
*!
*!      Function: lfOpnFiles
*!
*!**************************************************************************
*E300692,5 Add a function to open files
FUNCTION lfOpnFiles
PARAMETER lcFilToOpn, lcTagToOpn
IF FILE(lcDataDir + lcFilToOpn + '.DBF')
  IF USED(lcFilToOpn)
    USE IN (lcFilToOpn)
  ENDIF  
  SELECT 0
  USE (lcDataDir + lcFilToOpn) AGAIN ORDER TAG (lcTagToOpn) 
ENDIF

*!*************************************************************
*! Name      : lfGetLevel
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 09/06/1998
*! Purpose   : To get the module level in the modules tree
*! Reference : *E300993,1 AMM
*!*************************************************************
*! Called from : SMCMINF.PRG
*!*************************************************************
*! Calls       : lfGetLevel()
*!*************************************************************
*! Passed Parameters : lcApp      Application Name
*!                     lnArrNum   Application number in the array
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLevel()
*!*************************************************************
FUNCTION lfGetLevel
PARAMETERS lcApp,lnArrNum
PRIVATE lcApp,lnSeq,laChldSeq,lnCount,lnMax,i,lcMod,lnArrNo, lnModPs

RETURN 1

STORE 0 TO lnArrNo, lnCount
lcExact = SET('EXACT')
SET EXACT ON
lnModPs = ASUBSCRIPT(laReqMod,ASCAN(laReqMod,'*'+lcApp),1)
SET EXACT &lcExact
lcMod   = ALLTRIM(STRTRAN(laReqMod[lnModPs,2],'|'))

DO WHILE !EMPTY(lcMod)
  lnCount = lnCount + 1
  lcExact = SET('EXACT')
  SET EXACT ON
  lnArrNo = ASUBSCRIPT(laTempArr,ASCAN(laTempArr,LEFT(lcMod,2)),1)
  SET EXACT &lcExact
  
  DIMENSION laChldSeq[lnCount]
  laChldSeq[lnCount] = lfGetLevel(LEFT(lcMod,2),lnArrNo)
  lcMod=SUBSTR(lcMod,3)
ENDDO

IF lnCount = 0
  RETURN 1
ELSE
  lnMax = 0
  IF TYPE('laChldSeq')='N'
    FOR i = 1 TO ALEN(laChldSeq)  
      lnMax = MAX(lnMax,laChldSeq[i]) 
    ENDFOR
    RETURN lnMax + 1
  ENDIF  
ENDIF  


*!*************************************************************
*! Name      : lfGetFC
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 09/06/1998
*! Purpose   : Copy or remove the form codes from the DBFS form 
*!             code tables based on the form codes files in the 
*!             sysfiles directory.
*! Reference : *E301065,1 AMM
*!*************************************************************
*! Called from : SMCMINF.PRG
*!*************************************************************
*! Calls       : lfGetLevel()
*!*************************************************************
*! Passed Parameters : lcApp      Application Name
*!                     lnArrNum   Application number in the array
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetFC("AR","W:\aria27\dbfs\",'A')
*!*************************************************************
FUNCTION lfGetFC
PARAMETERS lcMod2Ins,lcDataDr,lcAddDel
IF FILE(lcDataDr+'FORMCDHD.DBF') .AND. ;
      FILE(lcDataDr+'FORMCDHD.DBF') 
  lnAlias = SELECT(0)
  lcFCDHD = gfTempName()
  lcFCDDT = gfTempName()
  USE (lcDataDr+'FORMCDHD.DBF') AGAIN ALIAS (lcFCDHD) ORDER TAG Formcdhd IN 0
  USE (lcDataDr+'FORMCDDT.DBF') AGAIN ALIAS (lcFCDDT) ORDER TAG Formcddt IN 0

  llOpSYREP = gfOpenFile(gcSysHome+'SYDREPRT','CREP_ID','SH')
  llOpFDT   = gfOpenFile(gcSysHome+'SYFRMCDD','FORMCDDT','SH')
  llOpFHD   = gfOpenFile(gcSysHome+'SYFRMCDH','FORMCDHD','SH')

  *E301065,1 AMM If calling from the install function, Add records to dbfs' files
  IF lcAddDel = 'A'      
    SELECT SYFRMCDD
    SET RELATION TO SYFRMCDD.cRep_Id INTO SYDREPRT ADDITIVE      
    SELECT SYFRMCDH
    SET RELATION TO SYFRMCDH.cformmaj INTO SYFrmCdD ADDITIVE      
    SCAN 
      llRecIns = .F.
      SELECT SYFRMCDD
      *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN] 
      *SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
       SCAN WHILE cformmaj = SYFRMCDH.cformmaj FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40"
          SCATTER MEMVAR MEMO
          IF !SEEK(m.cformmaj+m.cformcode,lcFCDDT)
            INSERT INTO (lcFCDDT) FROM MEMVAR
          ELSE
            SELECT (lcFCDDT) 
            GATHER MEMVAR MEMO   
          ENDIF
          SELECT SYFRMCDD
          llRecIns = .T.
        ENDSCAN
      *E038033,1 HMA [END]
      IF llRecIns 
        SELECT SYFRMCDH
        SCATTER MEMVAR MEMO
        IF !SEEK(m.cformmaj,lcFCDHD)
          INSERT INTO (lcFCDHD) FROM MEMVAR
        ELSE
          SELECT (lcFCDHD)
          GATHER MEMVAR MEMO FIELDS EXCEPT cCurForm, mFormSets
        ENDIF
      ENDIF  
      SELECT SYFRMCDH
    ENDSCAN
  *E301065,1 AMM If calling from the uninstall function, Remove records 
  *E301065,1 AMM from dbfs' form code files
  ELSE
    SELECT (lcFCDDT)
    SET RELATION TO cRep_ID INTO SYDREPRT ADDITIVE      
    SELECT (lcFCDHD)
    SET RELATION TO cformmaj INTO (lcFCDDT) ADDITIVE      
    SET SKIP TO (lcFCDDT),SYDREPRT
    
    *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
    *SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods)
    SCAN FOR lcMod2Ins $ UPPER(SYDREPRT.mCallMods) .AND. SYDREPRT.cVer<>"A40" 
    *E038033,1 HMA [END]
      llDelMod = .T.
      *E301065,1 AMM IF this module is the only calling installed module
      lcCalMod = STRTRAN(SYDREPRT.mCallMods,lcMod2Ins)
      lcCalMod = STRTRAN(lcCalMod,',')
      IF !EMPTY(lcCalMod)
        FOR lnI = 1 TO LEN(lcCalMod)/2
          IF SUBSTR(lcCalMod,1,2) $ lcOldMod
            llDelMod = .F.
            EXIT
          ELSE
            lcCalMod = SUBSTR(lcCalMod,3)
          ENDIF
        ENDFOR
      ENDIF
      IF llDelMod
        SELECT (lcFCDDT)
        DELETE
      ENDIF
    ENDSCAN
    DELETE FOR EOF(lcFCDDT)      
  ENDIF      
  *E301065,1 AMM Close files
  IF llOpSYREP .AND. USED('SYDREPRT')
    USE IN SYDREPRT
  ENDIF
  USE IN (lcFcdHd)
  USE IN (lcFcdDt)
  IF llOpFHD .AND. USED('SYFRMCDH')
    USE IN SYFRMCDH
  ELSE 
    SELECT SYFRMCDH
    SET RELATION TO
  ENDIF
  IF llOpFDT .AND. USED('SYFRMCDD')
    USE IN SYFRMCDD
  ELSE
    SELECT SYFRMCDD
    SET RELATION TO
  ENDIF
  SELECT (lnAlias)
ENDIF



*Hesham (start)
*B603737,1 if sort the modules selected to be installed to make the communication module
*B603737,1 first module if selected
FUNCTION lfSortMod
IF SEEK("CM","SYDAPPL")
  lccmProm = "  "+sydappl.cApp_Id+" "+sydappl.cApp_name+" +"+IIF(sydappl.lSetReq,"T","F")
  lncmPos = ASCAN(laAllMod,lccmProm)
  IF lncmPos >0
    =ADEL(laAllMod,lncmPos)
    =AINS(laAllMod,1)
    laAllMod[1] = lccmProm
  ENDIF
ENDIF
*Hesham (end)

*!*************************************************************
*! Name      : lfvStates
*! Developer : NADER NABIL (NNA)
*! Date      : 09/27/2004
*! Purpose   : Validate USA/CANADA State
*!*************************************************************
*! Calls     : AriaBrow
*!*************************************************************
*! Passed Parameters  : Main Account State / Store State
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvStates()
*!*************************************************************
*!B038431,1
FUNCTION lfvStates
PRIVATE lcFile_Ttl, lcBrfields, lcStaCode

lcStaCode = PROPER(LOOKUP(SYCINT.CPARt4LAB,IIF(EMPTY(laData[13]),ALLTRIM(gcContCode),ladata[13]),sycint.ccont_code,'CCONTCODE'))

*-- if you select country code for USA or for CANADA.
IF ALLTRIM(ladata[8])='USA' OR ALLTRIM(ladata[8])='CANADA'
  *-- Open codes file first time you go to this function.
  gcDataDir  = gfGetDataDir(ALLTRIM(sycComp.cCom_dDir))
  IF !USED('CODES')
    =gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
  ENDIF
  *-- IF you did not found state code in codes file.
  IF !EMPTY(laData[6])  
    SET ORDER TO CODES IN CODES
    IF !SEEK('N'+PADR(laData[6],6)+'N'+'STATE','CODES')
      lnCurAlias = SELECT(0)
       *-- browse all country codes 
       SELECT CODES
       DECLARE laCodeFld[2]
       lcFile_Ttl = ALLTRIM(lcStaCode) + ' Codes'
       lcBrfields = 'cCode_No :H=ALLTRIM(lcStaCode)+" Code",cDiscrep:H="Description"'
      
       IF gfBrows(' "N" FOR cDefCode+cfld_name+ccode_no+cdiscrep = ;
         "NSTATE" AND cRltField="N"','cCode_No','laCodeFld')
         laData[6] = laCodeFld[1]
       ENDIF
       SELECT (lnCurAlias)
    ENDIF
  ENDIF    
ENDIF
RETURN


