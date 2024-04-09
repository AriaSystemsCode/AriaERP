*:************************************************************************
*: Program file  : SMFSYER.PRG
*: Program desc. : FISCAL YEAR
*:         System: Aria advantage series
*:         Module: SYSTEM MANAGER
*:      Developer: Reham Aly Alallamy
*:           Date: 1993-1995 
*:************************************************************************
*: Calls : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*: Modifications
*B600463,1 MAN 6/21/95 The company name is not output in the period screen under windows 
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300692,4 Hsh 01/07/1997 change the fiscal year,holidays file from system file to data file
*E300692,5 RENEE 07/02/1997 Move lfAddFsYer functions from SM.PRG program
*E301098,1 HSH 12/16/1998 Get company data path using gfGetDataDir(..)
*B602193,1 AMM 02/22/1999 Default the fiscal years descriptions
*E300789,1 Hsh 03/04/1999 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1                Change Any seeking in these files
*B606568,1 RAE 10/24/2002 Initialize the Start and End Dates for new company. 
*B038431,1 NNA 09/27/2004 Fix bug that when you try to add a new Day Off , you'll 
*B038431,1 NNA            find that this new day off isn't Saved
*:************************************************************************
*
PARAMETERS pcComp_ID
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc

DECLARE laKeyField [2,4],laBlanks[1,1],laComp[1,1]
DECLARE laTStat[4,1],laCompInfo[3,1],laTYears[4]
DECLARE laHolDates[1,1],laCompany[1]

laDefProc[9]  = .F.               && Use local Save procedure

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'COMPFYEAR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'COMPFYEAR'
laKeyField[2,4] = 1

*E300692,4 Hesham (Start)
lcDataDir  = ''       && Var to hold the company data path
*E300692,4 Hesham (End)
lcComID    = ''       && Var to accept company ID in Copy Dialog
lcCmFisYer = ''       && Var to accept Fiscal year in Copy Dialog
lcOldComID = ''       && Var to keep old ID before Copy from another comp.
lcComNam   = ''       && Var to accept company name

lcOldPrds  = ''       && Var to keep old value of no. of periods
lcOldStrt  = {}       && Var to keep old value of Start date
lcOldEnd   = {}       && Var to keep old value of End date
lcOldCurPr = ''       && Var to keep old value of Current period

lcFisDes   = ''       && Var to keep 'Perivious or Current or Next'
lcYer      = ''       && Var to hold text in one of the messages
lcFldNm    = ''       && Var to hold text in one of the messages

lcCursor   = ''
lc_TempHd  = ''       && Var to hold name of Holidays temp. file
lc_TempPR  = ''       && Var to hold name of Periods temp. file
lcIndExpHd = ''       && Var to hold index expression of Holidays file
lcIndTagHd = ''       && Var to hold tag name of Holidays file

llUpdHd    = .F.      && Flag to know is there any modification in Holidays
llUpdPr    = .F.      && Flag to know is there any modification in Periods

lnFcount   = 0        && Var to keep no. of Holidays' fields 
lcCompFis  = ''       && Var to hold Key fields for Periods file
lcOldHold  = ''       && Var to hold Key fields for Holidays file
llSavFlg   = .F.
llOldFlg   = .F.      && Flag to know that return cancel from lfAcptPrd

llNewPrds = .F.       && Flag to be used if creat periods for 
                      && first time or when chang periods or dates.
llDefPrds = .F.       && Flag to be used if creat default 
                      && periods is required.
llAcptPrd = .F.       && Flag to tell if the periods of the year 
                      && is accepted before for the same record.

llEndScr  = .F.       && Flag to know if finish & close this screen or not
llFrsPop  = .T.

lcPopText  = " "
ibComp     = 1
puComp     = "Select Company"
puComp_Id  = " "
lcComp     = " "
lcComp_Id  = " "

STORE 0 TO cbDay_1,cbDay_2,cbDay_3,cbDay_4,cbDay_5,cbDay_6,cbDay_7

IF !gfSetUp()    
  RETURN
ENDIF  
IF EMPTY(pcComp_ID) .AND. !WEXIST(lcBaseWind)
  =gfopenfile(gcSysHome+'SYCCOMP','CCOMP_ID')
   laCtrStat[10] = 'DISABLE'
  *** See if there is any companies available in the system. ***
  SELECT SYCCOMP
  SET FILTER TO EMPTY(syccomp.ccompprnt)
  GO TOP
  IF EOF()
    *** No companies available. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00189B00000","DIALOG","companies")
    glQuitting = .T.
    RETURN
  ENDIF
ENDIF

*E300692,1 Hesham El_Sheltawi (Start)

*E300692,1 Hesham El_Sheltawi (End)
*** To know if the program called from the menu ***
*** or called from another program
SELECT SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name;
      FROM (gcSysHome+'SYCCOMP');
      INTO ARRAY laCompany

IF !WEXIST(gcBaseWind)
  IF _WINDOWS
*E300692,4 Hesham (Start)  
    puComp = "Select Company"
*    puComp = " "    
*    DEFINE POPUP puComp_Id prompt field ;
    SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME scroll;
    FROM 4.166,15.5 TO 10,15.5+36.875;
    MESSAGE gfObj_msg()
*    ON SELECTION POPUP puComp_Id DO lfvData_1
*E300692,4 Hesham (End)
  ENDIF

  lcCursor  = gfTempName()
  lc_TempHd = gfTempName()    && Temporary file of the holiday
  lc_TempPR = gfTempName()    && Temporary file of the periods

  CREATE CURSOR (lcCursor) (mMisc M (10))
  APPEND BLANK
  
  ***
*E300692,4 Hesham (Start)    
*  SELECT SYCFSHLD
*  lcIndExpHd = lfGetIndExp()
*  lcIndTagHd = lfGetIndTag()
  
*  lnFcount = FCOUNT('SYCFSHLD') + 3     && Var to know no. of holidays fields 
*  DECLARE laBlanks[5,lnFcount]
  
  *** Build array data with blanks records ***
*  FOR lnCnt = 1 TO 5
*    laBlanks[lnCnt,1]  = CHR(255)
*    laBlanks[lnCnt,2]  = CHR(255)
*    laBlanks[lnCnt,3]  = {}
*    laBlanks[lnCnt,4]  = SPACE(40)
*    laBlanks[lnCnt,5]  = SPACE(12)
*    laBlanks[lnCnt,lnFcount-1] = 'S'      && To skip these records during saving
*  ENDFOR

*  SELECT SYCFISHD
*  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
*E300692,4 Hesham (End)    
ENDIF

IF !EMPTY(pcComp_ID)
  lcComp_ID  = pcComp_ID
  IF SEEK(lcComp_ID,"SYCCOMP")
    puComp   = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
   *E300692,4 Hesham (Start)      
   *E300692,4 call the funtion to open the company fiscal year & priod files
    laData[1] = lcComp_ID
    =lfOpenCmpData()  
    *E300692,4 Hesham (End)  
  ENDIF
  
  llComIdFlg = .T.
  lcModal    = "MODAL"
ELSE
  lcComp_Id  = " "
  *E300692,4 Hesham (Start)  
*  puComp     = " "  
  puComp     = "Select Company"
  *E300692,4 Hesham (End)  
  lcComp     = " "
  llComIdFlg = .F.
  lcModal    = " "
ENDIF  
*E300692,4 Hesham (Start)
*E300692,4 change the base file of the program to ignore any errors
*E300692,4 in select mode
lcBaseFile = IIF(laScrMode[1],'SYCCOMP','SYCFISHD')
*E300692,4 Hesham (End)
llDoLocal = IIF(llComIdFlg,.T.,.F.)

laCtrStat[7] = IIF(laScrMode[2] .AND. laData[3] <> 'H',"ENABLE","DISABLE")
laCtrStat[8] = "DISABLE"

*** Set the non working days for each company ***
FOR lnCount = 1 TO 7
  lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(ALLTRIM(STR(lnCount)) $ laData[10],1,0)
ENDFOR

IF EMPTY(pcComp_ID)
  SELECT SYCCOMP
  SET FILTER TO EMPTY(syccomp.ccompprnt)
ENDIF

*E300692,4 Hesham (Start)
*SELECT SYCFISHD  
*E300692,4 Hesham (End)

*E300683,4 Call *.SPR from screens directory
* DO Smfsyer.SPR    
DO (gcScrDir + gcWinAppl + '\Smfsyer.SPR')
*E300683,4 end   
SELECT SYCCOMP
SET FILTER TO

IF llEndScr
  glQuitting = .T.
ENDIF
 
IF glQuitting
  *** If exit from this program, erasing all the temp. files ***
*  RELEASE POPUP puComp_Id
 
  IF USED(lcCursor)
    USE IN ALIAS(lcCursor)
  ENDIF

  IF USED(lc_TempHd)
    USE IN ALIAS(lc_TempHd)
  ENDIF
  ERASE (gcWorkDir+lc_TempHd+".DBF")
  ERASE (gcWorkDir+lc_TempHd+".FPT")
  ERASE (gcWorkDir+lc_TempHd+".CDX")

  IF USED(lc_TempPr)
    USE IN ALIAS(lc_TempPr)
  ENDIF
  ERASE (gcWorkDir+lc_TempPr+".DBF")
  ERASE (gcWorkDir+lc_TempPr+".FPT")
  ERASE (gcWorkDir+lc_TempPr+".CDX")
  *E300692,4 Hesham (Start)
  IF USED('SYCFISHD')
    USE IN SYCFISHD
  ENDIF
  
  IF USED('SYCFSHLD')
    USE IN SYCFSHLD
  ENDIF
  
  IF USED('SYCFSPRD')
    USE IN SYCFSPRD
  ENDIF
  *E300692,4 Hesham (End)
ENDIF 
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow
SHOW GET pbBrws DISABLE   
SHOW GET pbDlt DISABLE          && Disable the DELETE button all the time

IF laScrMode[2] .AND. laData[3] <> 'H'
  SHOW GET pbEdt ENABLE
ELSE
  SHOW GET pbEdt DISABLE
ENDIF 

*** If the program called from another program ***
IF llComIdFlg
  llComIdFlg = .F.
  llSavFlg   = .T.
  laData[1]  = lcComp_ID
  IF SEEK(laData[1],"SYCCOMP")
    *B600463,1  Define the company name in both dos and winows
    lcComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
    DO CASE
      CASE _DOS
        *B600463,1 
        *lcComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
        =lfRefresh()
        SHOW GET ibComp DISABLE
      CASE _WINDOWS
        puComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
        SHOW GET puComp DISABLE
    ENDCASE
  ENDIF
  lcFisDes   = laTYears[2]
  SHOW GET laData[2] ENABLE
ENDIF

*** Set the non working days for each company ***
FOR lnCount = 1 TO 7
  lcObjName  = "cbDay_"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(ALLTRIM(STR(lnCount)) $ laData[10],1,0)
ENDFOR

DO CASE
  *** Select Mode
  CASE laScrMode[1]
    lccompfis = " "
    IF EMPTY(laData[1])
      STORE " " TO lcComp,puComp,lccomp_id
      *E300692,4 Hesham (Start)
      laCtrStat[10] = 'DISABLE'
      SHOW GET pbBrws DISABLE
      pucomp = 'Select Company'
      *E300692,4 Hesham (End)
      laHolDates = {}
      lcFisDes   = ''
      llAcptPrd  = .F.
      llUpdHd    = .F.
      llUpdPr    = .F.
      SHOW GET pbPeriod  DISABLE
      SHOW GET pbNotes   DISABLE
      SHOW GET pbHoliDay DISABLE
    ENDIF
  *** View Mode
  CASE laScrMode[2] 
    IF SEEK(laData[1],"SYCCOMP")
*B600463,1
*      DO CASE 
*        CASE _DOS
          *B600463,1 Defining the Co. name in both DOS and Windows
          lcComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
*B600463,1          
*        CASE _WINDOWS
      *B600463,1 
      IF _WINDOWS  
          puComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_Name
      ENDIF
*B600463,1
*      ENDCASE
    ENDIF
    
    lcComp_Id  = laData[1]

    laHolDates = {}
*E300789,1 Hesham (Start)    
    *lcOldHold  = laData[1]+laData[2]
    lcOldHold  = laData[2]    
*E300789,1 Hesham (End)    
    *E300692,4 Hesham (Start)
*    SELECT sycfshld.dFshhdate ;
           FROM (gcsyshome+"sycfshld") ;
           WHERE &lcIndExpHd = laData[1]+laData[2];
           INTO ARRAY laHolDates

*E300789,1 Hesham (Start)    
     *SELECT dFshhdate ;
           FROM (lcDataDir+"fshld") ;
           WHERE &lcIndExpHd = laData[1]+laData[2];
           INTO ARRAY laHolDates

     SELECT dFshhdate ;
           FROM (lcDataDir+"fshld") ;
           WHERE &lcIndExpHd = laData[2];
           INTO ARRAY laHolDates

*E300789,1 Hesham (End)

*    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (gcSyshome+"sycfshld"),&lcCursor ;
           INTO DBF (gcWorkDir+lc_TempHd);
           WHERE &lcIndExpHd = laData[1]+laData[2]	;
           DISTINCT
 
*E300789,1 Hesham (Start)
    *SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (lcDataDir+"fshld"),&lcCursor ;
           INTO DBF (gcWorkDir+lc_TempHd);
           WHERE &lcIndExpHd = laData[1]+laData[2]	;
           DISTINCT

    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus' ;
           FROM (lcDataDir+"fshld"),&lcCursor ;
           INTO DBF (gcWorkDir+lc_TempHd);
           WHERE &lcIndExpHd = laData[2]	;
           DISTINCT

*E300789,1 Hesham (End)           
    *E300692,4 Hesham (End)
    INDEX ON &lcIndExpHd.&lcIndTagHd   && Indexing the temp file so any modification
                                       && will get the right position

    lcCompFis = laData[1]+laData[2]
   *E300692,4 Hesham (Start)    
*    SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
           FROM (gcsyshome+"sycFSPRD") ;
           WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = laData[1]+laData[2];
           INTO DBF (gcWorkDir+lc_TempPR)
    *E300789,1 Hesham (Start)   
    *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
           FROM (lcDataDir+"FSPRD") ;
           WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = laData[1]+laData[2];
           INTO DBF (gcWorkDir+lc_TempPR)

    SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
           FROM (lcDataDir+"FSPRD") ;
           WHERE CFISFYEAR+CFSPPRDID = laData[2];
           INTO DBF (gcWorkDir+lc_TempPR)

    *E300789,1 Hesham (End)       
    *E300692,4 Hesham (End)           
    SELECT SYCFISHD
    lnYer     = IIF(AT(laData[3],"PCNH") = 0 , 2 , AT(laData[3],"PCNH"))
    lcFisDes  = laTYears[lnYer]
    llAcptPrd = .T.
    llUpdHd   = .F.
    llUpdPr   = .F.
    SHOW GET pbPeriod  ENABLE
    SHOW GET pbNotes   ENABLE
    SHOW GET pbHoliDay ENABLE

  *** Edit Mode
  CASE laScrMode[3]
    IF SYCFISHD.cFisystat <> 'C'
      SHOW GET laData[5] DISABLE
    ENDIF
    SHOW GET laData[6] DISABLE
    SHOW GET laData[7] DISABLE
    
  *** Add Mode
  CASE laScrMode[4] AND EMPTY(laData[5])
    lcFisDes  = laTYears[2]
    laData[3] = 'C'
    llAcptPrd = .F.
    llUpdHd   = .F.
    llUpdPr   = .F.
    SHOW GET laData[5] DISABLE
    SHOW GET pbPeriod  DISABLE
    SHOW GET pbHoliDay DISABLE
ENDCASE
*E300692,4 Hesham (Start)
lcBaseFile = IIF(laScrMode[1],'SYCCOMP','SYCFISHD')
*E300692,4 Hesham (End)
SHOW GET lcFisDes DISABLE
 
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1
*E300692,4 Hesham (Start)
IF pucomp = 'Select Company'  OR SUBSTR(pucomp,1,2) == lcComp_Id
  RETURN
ENDIF
*E300692,4 Hesham (End)
*puComp    = SYCCOMP.cComp_Id+" "+SYCCOMP.CCOM_NAME
lcComp_Id = SUBSTR(pucomp,1,2) &&SYCCOMP.CCOMP_ID

*SHOW GET puComp

IF !EMPTY(lcComp_Id)
  laData[1] = lcComp_Id
 
  IF SEEK(laData[1],"SYCCOMP")
    laData[2] = sycComp.cCurr_yer
  ELSE
    laData[2] = SPACE(4)
  ENDIF
  *B606568,1 RAE Initialize the Start and End Dates for new company. [start]
  IF EMPTY(laData[2])
    STORE {} TO laData[6] , laData[7]
  ENDIF
  *B606568,1 RAE [end]
  *E300692,4 Hesham (Start)
  *E300692,4 open all needed files for the company from the company 
  *E300692,4 data dir.
  =lfOpenCmpData()
  *E300692,4 Hesham (End)
  
  DO CASE
    CASE !EMPTY(laData[1]) .AND. EMPTY(laData[2])
      *** The company fiscal years have not been set up yet. ***
      *** Setup or copy from another company?                ***
      *** <  Setup  > - <  Copy  > - <  Cancel  > ***
      lnOption = gfModalGen("QRM00082B00019","DIALOG")
      DO CASE
        CASE lnOption = 1                && Setup
          DO CASE
            CASE _DOS
              SHOW GET ibComp    DISABLE
            CASE _WINDOWS
              SHOW GET puComp    DISABLE
          ENDCASE
          lcFisDes = laTYears[2]
          SHOW GET lcFisDes DISABLE
          SHOW GET laData[2] ENABLE
          _CUROBJ = OBJNUM(laData[2])
        CASE lnOption = 2                && Copy
          lcOldComID = laData[1]
          lcComID    = ''
          lcCmFisYer = ''
          =lfCopyComp()                  && Function to copy data from another comp.
          SHOW GETS
        CASE lnOption = 3                && Cancel
          DO CASE
            CASE _DOS
              SHOW GET ibComp ENABLE
              _CUROBJ = OBJNUM(ibComp)
            CASE _WINDOWS
              SHOW GET puComp ENABLE
              _CUROBJ = OBJNUM(puComp)
          ENDCASE
      ENDCASE
    CASE !EMPTY(laData[1]) .AND. !EMPTY(laData[2])
      *E300789,1 Hesham (Start)
      *SEEK laData[1]+laData[2]
      SEEK laData[2]
      *E300789,1 Hesham (End)
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
  ENDCASE
ELSE
  DO CASE
    CASE _DOS
      SHOW GET ibComp ENABLE
      _CUROBJ    = OBJNUM(ibComp)
    CASE _WINDOWS
      SHOW GET puComp ENABLE
      _CUROBJ    = OBJNUM(puComp)
  ENDCASE
ENDIF

IF _WINDOWS
  DEACTIVATE POPUP puComp_Id
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2
 
IF !EMPTY(laData[2]) .AND. LASTKEY() = 13
  laData[1] = IIF(EMPTY(laData[1]),lcComp_Id,laData[1])
  IF EMPTY(laData[1])
    *** You have to select company first. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00192B00000","DIALOG")
    laData[2] = SPACE(4)
    DO CASE
      CASE _DOS
        _CUROBJ   = OBJNUM(ibComp)
      CASE _WINDOWS
        _CUROBJ   = OBJNUM(puComp)
    ENDCASE
    RETURN
  ENDIF
  
  IF !BETWEEN(VAL(laData[2]),1900,9998)
    laData[2] = SPACE(4)
    _CUROBJ   = OBJNUM(laData[2])
    RETURN
  ELSE
    *E300789,1 Hesham (Start)
    *IF SEEK(ALLTRIM(laData[1])+ALLTRIM(laData[2]))
    IF SEEK(ALLTRIM(laData[2]))
    *E300789,1 Hesham (End)
      SCATTER FIELDS &lcScFields MEMO TO laData
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ELSE
      lcFisYears = ALLTRIM(STR(VAL(laData[2])-1))+','+ALLTRIM(laData[2]);
                   +','+ALLTRIM(STR(VAL(laData[2])+1))
      *** Fiscal years are ð.  Create default fiscal years? ***
      *** <  Yes  > - <  No  > - <  Reenter  > ***
      lnOption = gfModalGen("QRM00081B00020","DIALOG",lcFisYears)
      DO CASE
        CASE lnOption = 1               && set the default values
          laData[4]    = '12'
          laData[5]    = '01'
          lcSavDate    = SET("DATE")
          SET DATE American
          laData[6]    = CTOD('01/01/'+ALLTRIM(laData[2]))
          laData[7]    = CTOD('12/31/'+ALLTRIM(laData[2]))
          SET DATE &lcSavDate
          *B602193,1 AMM Default current year short and long descriptions
          lcCen = SET('CENTURY')
          SET CENTURY ON
          laData[8] = 'Fis. year '+laData[2]
          laData[9] = 'Fiscal year '+laData[2]+ SPACE(1) + '(' + ;
                       DTOC(laData[6]) +'-' +DTOC(laData[7]) + ')'
          SET CENTURY &lcCen
          *B602193,1 AMM end          
          laData[10]   = '67'
          laScrMode    = .F.
          laScrMode[4] = .T.
          SHOW GETS
        CASE lnOption = 2
          laScrMode    = .F.
          laScrMode[4] = .T.
          SHOW GETS
        CASE lnOption = 3
          laData[2] = SPACE(4)
          SHOW GET laData[2] ENABLE
          _CUROBJ = OBJNUM(laData[2])
      ENDCASE
    ENDIF
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_4
*!
*!**************************************************************************
*
FUNCTION lfvData_4

IF LASTKEY() = 27
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF

lcFldNm  = 'No. of periods'
lnNoPrd  = VAL(laData[4])
lnCurPrd = VAL(laData[5])

IF lnCurPrd <> 0 .AND. lnNoPrd  < lnCurPrd .AND. lnNoPrd <> 0
  *** No. of periods cannot be less than ***
  *** the value of current period...!    ***
  *** <  Ok  > ***
  = gfModalGen("QRM00062B00000","DIALOG")
  laData[4] = lcOldPrds
  _CUROBJ = OBJNUM (laData[4])
ELSE
  IF lnNoPrd <= 0 .OR. lnNoPrd > 13
    *** Period value must be between 1 and ð...! ***
    *** <  Ok  > ***
    = gfModalGen("QRM00063B00000","DIALOG",'13')
    laData[4] = lcOldPrds
    _CUROBJ   = OBJNUM (laData[4])
  ELSE
    laData[4] = RIGHT("0"+ALLTRIM(laData[4]),2)  
    IF lcOldPrds <> laData[4]
      =lfvAcptPrd()          && To know if accept to change periods or not.
      IF llOldFlg
        llOldFlg = .F.
        laData[4] = lcOldPrds
      ENDIF
    ENDIF  
    SHOW GET laData[5] ENABLE
  ENDIF
ENDIF

SHOW GET laData[4]

*!**************************************************************************
*!
*!      Function: lfwData_5
*!
*!**************************************************************************
*
FUNCTION lfwData_5

lcOldCurPr = laData[5]

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*
FUNCTION lfvData_5

IF LASTKEY() = 27
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF

lnNoPrd  = VAL(laData[4])
lnCurPrd = VAL(laData[5])

IF lnCurPrd <= 0 .OR. lnCurPrd > lnNoPrd
  *** Period value must be between 1 and ð...! ***
  *** <  Ok  > ***
  =gfModalGen("QRM00063B00000","DIALOG",laData[4])
  laData[5] = lcOldCurPr
  _CUROBJ = OBJNUM (laData[5])
ELSE 
  laData[5] = RIGHT("0"+ALLTRIM(laData[5]),2)
ENDIF

DO CASE
  CASE laScrMode[3] .AND. llAcptPrd
    SELECT (lc_TempPR)
    GO TOP
    REPLACE ALL cStatus   WITH IIF(AT(cStatus,"SAM") > 0,;
                               SUBSTR("MAM",AT(cStatus,"SAM"),1),"S");
                lFspclsds WITH IIF(VAL(cFspprdid) < VAL(laData[5]),;
                               .T. , .F.)
    llUpdPr = .T.
    SELECT SYCFISHD
  CASE laScrMode[4] .AND. llAcptPrd
    SELECT (lc_TempPR)
    GO TOP
    SCAN
      REPLACE lFspclsds WITH IIF(VAL(cFspprdid) < VAL(laData[5]),.T.,.F.)
    ENDSCAN
    SELECT SYCFISHD
ENDCASE
SHOW GET laData[5] 

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*
FUNCTION lfvData_6

IF LASTKEY() = 27
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF
 
lcFldNm = 'Start date'
lcYer   = ALLTRIM(laData[2]) + ' or ' + ALLTRIM(STR(VAL(laData[2])-1))

DO CASE
  CASE laData[6] >= laData[7] .AND. !EMPTY(laData[7])
    *** End date must be greater than start date. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00069B00000","DIALOG")
    laData[6] = lcOldStrt
    _CUROBJ = OBJNUM (laData[6])
  CASE GOMONTH(laData[6],12) < laData[7] .AND. !EMPTY(laData[7])
    *** Fiscal year cannot exceed twelve months. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00070B00000","DIALOG")
    laData[6] = lcOldStrt
    _CUROBJ = OBJNUM (laData[6])
  CASE YEAR(laData[6]) = VAL(laData[2]) .OR. ;
       YEAR(laData[6]) = VAL(laData[2]) - 1
    IF lcOldStrt <> laData[6]
      =lfvAcptPrd()         && To know if accept to change periods or not.
      IF llOldFlg
        llOldFlg = .F.
        laData[6] = lcOldStrt
      ENDIF
    ENDIF
    =lfvChcDate()             && To know if there is any holidays not
                              && within the start & end date..
    SHOW GET laData[6] 
  OTHERWISE
    *** Start date can only be in year ð. ***
    *** <  Ok  > ***
    =gfModalGen("QRM00067B00000","DIALOG",lcYer)
    laData[6] = lcOldStrt
    _CUROBJ = OBJNUM (laData[6])
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
 FUNCTION lfvData_7

IF LASTKEY() = 27
  _CUROBJ = OBJNUM(pbCls)
  RETURN
ENDIF

lcFldNm = 'End date'

DO CASE
  CASE YEAR(laData[7]) <> VAL(laData[2])
    *** End date must be in ð. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00068B00000","DIALOG",laData[2])
    laData[7] = lcOldEnd
    _CUROBJ = OBJNUM (laData[7])
  CASE laData[7] <= laData[6]
    *** End date must be greater than start date. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00069B00000","DIALOG")
    laData[7] = lcOldEnd
    _CUROBJ = OBJNUM (laData[7])
  CASE GOMONTH(laData[6],12) < laData[7] .AND. !EMPTY(laData[6])
    *** Fiscal year cannot exceed twelve months. ***
    *** <  Ok  > ***
    = gfModalGen("QRM00070B00000","DIALOG")
    laData[7] = lcOldEnd
    _CUROBJ = OBJNUM (laData[7])
  OTHERWISE
    IF lcOldEnd <> laData[7]
      =lfvAcptPrd()         && To know if accept to change periods or not
      IF llOldFlg
        llOldFlg = .F.
        laData[7] = lcOldEnd
      ENDIF
    ENDIF
    =lfvChcDate()             && To know if there is any holidays not
                              && within the start & end date..
ENDCASE

SHOW GET laData[7]
SHOW GET pbPeriod  ENABLE
SHOW GET pbHoliDay ENABLE

*!**************************************************************************
*!
*!      Function: lfvPeriod
*!
*!**************************************************************************
*
FUNCTION lfvPeriod

*E300683,4 Call programs from PRGS directory
*DO SMFSPRD.PRG 
DO (gcAppHome + gcWinAppl + '\SMFSPRD ') 
*E300683,4 end

*!**************************************************************************
*!
*!      Function: lfvAddDay
*!
*!**************************************************************************
*
FUNCTION lfvAddDay

*B038431,1 NNA 09/27/2004 (Begin) Use the 'ALLTRIM(laData[10]' instead of laData[10] only
*B038431,1     because when we add a new day off , the new laData[10] Become like '67     5'
*B038431,1     so it can't save
*laData[10] = IIF(EVALUATE(SYS(18)) = 1,;
               laData[10]+RIGHT(SYS(18),1),;
               STUFF(laData[10],AT(RIGHT(SYS(18),1),laData[10]),1,''))
laData[10] = IIF(EVALUATE(SYS(18)) = 1,;
               ALLTRIM(laData[10])+RIGHT(SYS(18),1),;
               STUFF(laData[10],AT(RIGHT(SYS(18),1),laData[10]),1,''))
*B038431,1 NNA (End)

laData[10] = ALLTRIM(laData[10])

=gfUpdate()

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
* This procedure replaces the default Save procedure for the push button
* "SAVE"
PROCEDURE lpSavScr
EXTERNAL ARRAY laScrMode    

IF EMPTY(laData[4])
  *** You have to enter no. of periods for this fiscal year. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00204B00000","DIALOG")
  _CUROBJ = OBJNUM(laData[4])
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[5])
  *** You have to enter current period for this fiscal year. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00205B00000","DIALOG")
  _CUROBJ = OBJNUM(laData[5])
  llCSave = .F.
  RETURN
ENDIF
IF EMPTY(laData[6]) .OR. EMPTY(laData[7])
  *** You cannot save this fiscal year without ***
  *** entering the start and the end date  for ***
  *** this year...
  *** <  Ok  > ***
  =gfModalGen("TRM00203B00000","DIALOG")
  _CUROBJ = IIF(EMPTY(laData[6]),OBJNUM(laData[6]),OBJNUM(laData[7]))
  llCSave = .F.
  RETURN
ENDIF

IF laScrMode[4]             && Add mode
  
  DO WHILE  !llAcptPrd
    *** You have to add periods for the fiscal year ***
    *** ð before saving or cancel the session. ***
    *** <  Ok  > - <  Cancel  > ***
    IF gfModalGen("TRM00084B00030","DIALOG",laData[2]) = 1
      *E300683,4 Call programs from PRGS directory
      *DO SMFSPRD.PRG 
      DO (gcAppHome + gcWinAppl + '\SMFSPRD ') 
      *E300683,4 end
    ELSE
      _CUROBJ = OBJNUM(pbCls)
      llCSave = .F.
      RETURN
    ENDIF
  ENDDO

  SELECT SYCFISHD
  
  =lfAddFsYer('laData[2]','laData[3]','laData[6]','laData[7]',;
              lc_TempPR,VAL(laData[5]),VAL(laData[4]))

  IF llUpdHd
    llUpdHd = .F.
    =gfTmp2Mast('SYCFSHLD',lc_TempHd,;
                "Save all the holidays for the current year...")
  ENDIF

ELSE

  *E300789,1 Hesham (Start)
  IF lcBaseFile = 'SYCFISHD'
    lcScattFields = STRTRAN(lcScFields,'CCOMP_ID,')
    =ACOPY(laData,laFsYear)
    =ADEL(laFsYear,1)
    GATHER FROM laFsYear FIELDS &lcScattFields MEMO         
  ELSE
    GATHER FROM laData FIELDS &lcScFields MEMO     
  ENDIF
  *E300789,1 Hesham (End)
  =gfAdd_Info()
  
  IF llUpdHd
    llUpdHd = .F.
    =gfTmp2Mast('SYCFSHLD',lc_TempHd,;
                "Save all the holidays for the current year...")
  ENDIF

  IF llUpdPr
    llUpdPr = .F.
    =gfTmp2Mast('SYCFSPRD',lc_TempPr,;
                "Save periods for "+lcFisDes+" year..")
  ENDIF
ENDIF  

SELECT SYCFISHD

IF llSavFlg
  llSavFis = .T.
  llEndScr = .T.
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      Function: lfvAcptPrd
*!
*!**************************************************************************
*
FUNCTION lfvAcptPrd

IF llAcptPrd
  lnOption =gfModalGen("QRM00074B00018","ALERT",lcFldNm)
  llOldFlg    = .T.
  DO CASE
    CASE lnOption = 1               && Reenter Periods
      llAcptPrd   = .F.
      llNewPrds   = .T.
      llDefPrds   = .F.
      *E300683,4 Call programs from PRGS directory
      *DO SMFSPRD.PRG 
      DO (gcAppHome + gcWinAppl + '\SMFSPRD ') 
      *E300683,4 end
    CASE lnOption = 3               && Accept Default
      llAcptPrd   = .F.
      llNewPrds   = .T.
      llDefPrds   = .T.
      *E300683,4 Call programs from PRGS directory
      *DO SMFSPRD.PRG 
      DO (gcAppHome + gcWinAppl + '\SMFSPRD ') 
      *E300683,4 end
  ENDCASE
ENDIF

*!**************************************************************************
*!
*!      Function: lfCopyComp
*!
*!**************************************************************************
*
FUNCTION lfCopyComp

SELECT SYCFISHD
lnSavRec = IIF(RECNO()>RECCOUNT(),0,RECNO())
GO TOP
llEof = EOF()
IF lnSavRec > 0
  GO lnSavRec
ENDIF

IF !llEof
  *E300683,4 Call *.SPR from screens directory
  * DO SMCOPCM.SPR    
  DO (gcScrDir + gcWinAppl + '\SMCOPCM.SPR')
  *E300683,4 end   
ELSE
  =gfModalGen("TRM00086B00000","DIALOG")
  DO CASE
    CASE _DOS
      SHOW GET ibComp DISABLE
    CASE _WINDOWS
      SHOW GET puComp DISABLE
  ENDCASE
  lcFisDes = laTYears[2]
  SHOW GET lcFisDes DISABLE
  SHOW GET laData[2] ENABLE
  _CUROBJ = OBJNUM(laData[2])
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvComID
*!
*!**************************************************************************
*
FUNCTION lfvComID

IF lcComID  = "?"
 lcCmFisYer = "?"
 _CUROBJ    = OBJNUM(lcCmFisYer)
 KEYBOARD "{ENTER}"
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCmFisYr
*!
*!**************************************************************************
*
FUNCTION lfvCmFisYr

IF !EMPTY(lcComID) .AND. !EMPTY(lcCmFisYer)
  IF SEEK(lcComID+lcCmFisYer)
    =lfColData()              && Collect data from another company.
  ELSE
    GO TOP
    lcSaveBrow  = lcBrFields
    lcBrFields  = "cComp_id :H='Company ID',cFisfyear :H='Fiscal Year',cFisystat :H='Fiscal State'"
   
    lcOld_ttl   = lcFile_ttl
    lcFile_ttl  = "Fiscal Year Information"

    laData[1]   = ''
    DO gfBrows

    lcBrFields = lcSaveBrow 
    lcFile_ttl = lcOld_ttl

    IF EMPTY(laData[1])
      lcComID    = '  '
      lcCmFisYer = '    '
      SHOW GET lcComID ENABLE
      _CUROBJ = OBJNUM(lcComID)
    ELSE
      =lfColData()              && Collect data from another company
    ENDIF
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfCOlData
*!
*!**************************************************************************
*
FUNCTION lfColData

*** Collect data from another company ***
IF SEEK(lcComID,"SYCCOMP")
  lcComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_name
  puComp = SYCCOMP.cComp_Id+" "+SYCCOMP.cCom_name
  
  *** Collect data from the fiscal header file ***
  SCATTER FIELDS &lcScFields MEMO TO laData
  
  *** Collect all Periods data in temp. file ***
  *E300789,1 Hesham (Start)
  *SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
         FROM (lcDataDir+"FSPRD") ;
         WHERE CCOMP_ID+CFISFYEAR+CFSPPRDID = laData[1]+laData[2];
         INTO DBF (gcWorkDir+lc_TempPR)

  SELECT *,RECNO() AS 'nRecNo', "S" AS 'cStatus';
         FROM (lcDataDir+"FSPRD") ;
         WHERE CFISFYEAR+CFSPPRDID = laData[2];
         INTO DBF (gcWorkDir+lc_TempPR)

  *E300789,1 Hesham (End)
  *** Replace company ID in all records with the old Company ID ***
  *E300789,1 Hesham (Start)
  *SELECT (lc_TempPR)
  *GO TOP
  *REPLACE ALL CCOMP_ID WITH lcOldComID
  *E300789,1 Hesham (End)
  
  SELECT SYCFISHD
  lcComp_Id    = lcOldComID
  laData[1]    = lcOldComID
  *E300789,1 Hesham (Start)
  *lcCompFis    = laData[1]+laData[2]
  lcCompFis    = laData[2]  
  *E300789,1 Hesham (End)
  lcFisDes     = laTYears[2]
  llAcptPrd    = .T.
  llUpdPr      = .T.
  laScrMode    = .F.
  laScrMode[4] = .T.       && Go to Add mode
  
  _CUROBJ = OBJNUM(pbOk)
  KEYBOARD "{ENTER}"
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel

lcComp_Id = lcOldComID
laData[1] = lcOldComID
DO CASE
  CASE _DOS
    SHOW GET ibComp ENABLE
    _CUROBJ = OBJNUM(ibComp)
  CASE _WINDOWS
    SHOW GET puComp ENABLE
    _CUROBJ = OBJNUM(puComp)
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvChcDate
*!
*!**************************************************************************
*
FUNCTION lfvChcDate
*** To know if there is any holidays not ***
*** within the start & end date..        ***

IF !EMPTY(laHolDates)
  SELECT (lc_TempHd)
  FOR lnCnt = 1 TO ALEN(laHolDates,1)
    IF !BETWEEN(laHolDates[lnCnt],laData[6],laData[7])
      IF SEEK(laData[1]+laData[2]+DTOS(laHolDates[lnCnt]))
        lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)   && Delete
        REPLACE cStatus WITH lcStatus
        DELETE
        llUpdHd   = .T.
        laHolDates[lnCnt] = {}
      ENDIF
    ENDIF
  ENDFOR
ENDIF

*E300692,4 Hesham (Start)
*!**************************************************************************
*!
*!      Function: lfOpenCmpData
*!
*!**************************************************************************
* function to open all the data files from the company dir.
FUNCTION lfOpenCmpData
IF !EMPTY(laData[1])
  
  lcDataDir = ALLT(LOOKUP(SYCCOMP.CCOM_DDIR,laData[1],syccomp.ccomp_id,'Ccomp_id'))
  *E301098,1 Hesham (Start)
*  IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
*      UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
*    lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
*  ENDIF
  lcDataDir = gfGetDataDir(lcDataDir)
  *E301098,1 Hesham (End)
  IF USED('SYCFISHD')
    USE IN SYCFISHD
  ENDIF
  USE (lcDataDir+'FISHD') IN 0 AGAIN ALIAS SYCFISHD ORDER TAG COMPFYEAR
  
  IF USED('SYCFSHLD')
    USE IN SYCFSHLD
  ENDIF
  USE (lcDataDir+'FSHLD') IN 0 AGAIN ALIAS SYCFSHLD ORDER TAG COMFYRHDAT
  
  IF USED('SYCFSPRD')
    USE IN SYCFSPRD
  ENDIF
  USE (lcDataDir+'FSPRD') IN 0 AGAIN ALIAS SYCFSPRD ORDER TAG Comfyrprdi
  

  SELECT SYCFSHLD
  lcIndExpHd = lfGetIndExp()
  lcIndTagHd = lfGetIndTag()
  
  lnFcount = FCOUNT('SYCFSHLD') + 3     && Var to know no. of holidays fields 
  DECLARE laBlanks[5,lnFcount]
  
  *** Build array data with blanks records ***
  FOR lnCnt = 1 TO 5
    laBlanks[lnCnt,1]  = CHR(255)
    laBlanks[lnCnt,2]  = CHR(255)
    laBlanks[lnCnt,3]  = {}
    laBlanks[lnCnt,4]  = SPACE(40)
    laBlanks[lnCnt,5]  = SPACE(12)
    laBlanks[lnCnt,lnFcount-1] = 'S'      && To skip these records during saving
  ENDFOR

  SELECT SYCFISHD
*  SCATTER FIELDS &lcScFields MEMO TO laData &&BLANK
ENDIF
*E300692,4 Hesham (End)




*!********************************************************************
*!
*!              Function: lfAddFsYer
*!
*!********************************************************************
*
FUNCTION lfAddFsYer
PARAMETERS lcCurrYear,lcYearStat,lcBegDate,lcEndDate,;
           lcPrdTpFil,lnCurPriod,lnNo_Per

lcSavAlias = ALIAS() 
lcSaveYear = &lcCurrYear
lcSaveStat = &lcYearStat
ldSaveBeg  = &lcBegDate
ldSaveEnd  = &lcEndDate


llLastFeb  = .F.
lnYearCurr = INT(VAL(lcSaveYear))
lnYear     = INT(VAL(lcSaveYear))-2
lnYearDays = (ldSaveEnd - ldSaveBeg) + 1
ldBegin    = GOMONTH(ldSaveBeg ,-24)
ldEnd      = GOMONTH(ldSaveEnd ,-24)

*** We have to stop on the company record in the company file before start
*** the update becouse we are updateing the fields of the fisical year file
*** And the Currunt year and period in the company file at the same GATHER
*** command or you may have the error "End of file incountered" in the
*** company file not in the fisical year file.

SELECT SYCCOMP
SEEK laData[1]

SELECT (lcPrdTpFil)

*B601298,1 This lines was added by HS [Begin]
llNotNor = .F.            &&Logical varible to know if the month Feb. is 29 days and ther is a priod with end date 02/28 
LOCATE FOR MONTH(dFsppendt) = 2 .AND. DAY(dFsppendt) = 28 
*B601298,1 IF Statmenyt to check if the month Feb. is 29 days and ther is
*B601298,1 a priod with end date 02/28 
IF FOUND() AND MOD(YEAR(dFsppendt),4) = 0
  llNotNor = .T.
ENDIF       &&End of the IF Statmenyt 
*B601298,1 This lines was added by HS [End]

*B601298,1 Ther is no need for this lines [Begin]
*GO TOP 
*SKIP

*IF (MONTH(dFsppendt) = 2) .AND. ;
*   (DAY  (dFsppendt) = IIF((MOD(lnYearCurr,4) = 0),29,28))
*  llLastFeb = .T.
*ENDIF
*B601298,1 Ther is no need for this lines [End]

GO TOP

REPLACE ALL cFisFYear  WITH ALLTRIM(STR(lnYear))   ;
            dFsppbgdt  WITH GOMONTH(dFsppbgdt , -24) ;
            dFsppendt  WITH GOMONTH(dFsppendt , -24)

DECLARE laYears[3]
laYears[1] = "previous"
laYears[2] = "current" 
laYears[3] = "next"

DECLARE laPerLock[lnNo_Per]
laPerLock = .F.
SCAN
  laPerLock[VAL(&lcPrdTpFil..cfspprdid)] = &lcPrdTpFil..lfsplocks
ENDSCAN
*B602193,1 AMM Save current year descriptions in two variables
lcShDesc = laData[8]
lcLngDes = laData[9]
*B602193,1 AMM end
FOR lnCount  = 1 TO 3
  SELECT SYCFISHD
  lnYear      = lnYear  + 1
  ldBegin     = GOMONTH(ldBegin ,12)
  ldEnd       = GOmONTH(ldEnd   ,12)
  &lcCurrYear = ALLTRIM(STR(lnYear))
  &lcYearStat = SUBSTR('PCN',lnCount) 
  &lcBegDate  = ldBegin 
  &lcEndDate  = ldEnd
  *B602193,1 AMM Default the previous and next years descriptions.
  lcCen = SET('CENTURY')
  SET CENTURY ON
  IF SUBSTR('PCN',lnCount) # 'C'
    laData[8] = 'Fis. year '+ ALLTRIM(STR(lnYear))
    laData[9] = 'Fiscal year '+ALLTRIM(STR(lnYear))+ SPACE(1) + '(' + ;
                DTOC(ldBegin) +'-' +DTOC(ldEnd) + ')'
  ELSE
    laData[8] = lcShDesc
    laData[9] = lcLngDes
   ENDIF
  SET CENTURY &lcCen
  *B602193,1 AMM end
  
  *E300789,1 Hesham (Start)
  APPEND BLANK
  IF lcBaseFile = 'SYCFISHD'
    lcScattFields = STRTRAN(lcScFields,'CCOMP_ID,')
    =ACOPY(laData,laFsYear)
    =ADEL(laFsYear,1)
    GATHER FROM laFsYear FIELDS &lcScattFields MEMO         
  ELSE
    GATHER FROM laData FIELDS &lcScFields MEMO     
  ENDIF
  *E300789,1 Hesham (End)
  =gfAdd_Info()
  SELECT (lcPrdTpFil)
  
*B601298,1 Changing the REPLACE Statment 
*B601298,1 change this line              dFsppendt WITH GOMONTH(dFsppendt ,12) +;
*B601298,1    and this line                       IIF(llLastFeb .AND. MOD(lnYear,4)=0 ,1,0);
*B601298,1   with this line             dFsppendt WITH GOMONTH(dFsppendt ,12); 
*  REPLACE ALL cFisFYear WITH ALLTRIM(STR(lnYear))   ;
*              dFsppbgdt WITH GOMONTH(dFsppbgdt ,12) ;
*              dFsppendt WITH GOMONTH(dFsppendt ,12) +;
*                             IIF(llLastFeb .AND. MOD(lnYear,4)=0 ,1,0);
*              lFspclsds WITH IIF(lnCount=1,.T.,IIF(lnCount=3,.F.,;
*                             IIF(VAL(cFspprdid)<lnCurPriod,.T.,.F.)));
*              lfsplocks WITH IIF(lnCount=1 .OR. lnCount=3,.F.,;
*                             laPerLock[VAL(&lcPrdTpFil..cfspprdid)]);
*              cStatus   WITH 'A'
  REPLACE ALL cFisFYear WITH ALLTRIM(STR(lnYear))   ;
              dFsppbgdt WITH GOMONTH(dFsppbgdt ,12) ;
              dFsppendt WITH GOMONTH(dFsppendt ,12) ;
              lFspclsds WITH IIF(lnCount=1,.T.,IIF(lnCount=3,.F.,;
                             IIF(VAL(cFspprdid)<lnCurPriod,.T.,.F.)));
              lfsplocks WITH IIF(lnCount=1 .OR. lnCount=3,.F.,;
                             laPerLock[VAL(&lcPrdTpFil..cfspprdid)]);
              cStatus   WITH 'A'


  *B601298,1 This lines was added by HS [Begin]
  LOCATE FOR MONTH(dFsppendt) = 2 .AND. DAY(dFsppendt) = 28 
  *B601298,1 IF Statmenyt to check if found and not the last record
  IF FOUND()    
    ldCrPrBeg = dFsppendt + 1        &&Varible to hold the next priod begin date
    *B601298,1 IF Statmenyt to check if Feb. is 29 day and that the user 
    *B601298,1 want the end date of this priod to be 02/29
    IF MOD(YEAR(dFsppendt),4) = 0 .AND. !llNotNor
      REPLACE dFsppendt WITH (dFsppendt + 1)
    ENDIF      &&End of the IF Statmenyt

    *B601298,1 IF Statmenyt to check if Feb. is 29 day and that the user 
    *B601298,1 want the end date of this priod to be 02/28
    IF llNotNor 
      SKIP 1
      *B601298,1 IF Statmenyt to check that the current record is not 
      *B601298,1 end of file
      IF !EOF()
        REPLACE dFsppbgdt WITH (ldCrPrBeg)
      ENDIF      &&End of the IF Statmenyt
    ENDIF      &&End of the IF Statmenyt
  ENDIF      &&End of the IF Statmenyt
  *B601298,1 This lines was added by HS [End]        

  =gfTmp2Mast('SYCFSPRD',lcPrdTpFil,;
              "Save periods for "+laYears[lnCount]+" year...")
ENDFOR

&lcCurrYear = lcSaveYear
&lcYearStat = lcSaveStat 

SELECT SYCCOMP
SEEK laData[1]
REPLACE cCurr_yer WITH laData[2]

SELECT (lcSavAlias)

