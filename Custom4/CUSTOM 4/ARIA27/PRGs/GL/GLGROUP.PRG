*:*************************************************************************
*:                                                                      ***
*: Procedure file: GLGROUP.PRG                                          ***
*:                                                                      ***
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Wadie                                           ***
*:      Copyright (c)                                                   ***
*:  Last modified: 01/09/1994                                           ***
*:                                                                      ***
*:  Procs & Fncts:                                                      ***
*:                                                                      ***
*:    PROCEDURE lpShow      *** Procdure to control screen modes        ***
*:    FUNCTION: lfvData_1   *** Validation for key field                ***
*:    FUNCTION: lfvGrdSeqno *** Validation for "nGrdSeqno" field        ***
*:    FUNCTION: lfvGrdStat  *** Validation for "cGrdStat" check box     *** 
*:    FUNCTION: lfvAddAct   *** Validation for Add Account option       ***
*:    FUNCTION: lfvRemAct   *** Validation for remove account option    ***
*:    FUNCTION: lfwActList  *** List Validation                         ***
*:    FUNCTION: lpSavScr    *** Save group processing                   ***
*:    FUNCTION: lpDelScr    *** Delete group processing                 ***
*:    FUNCTION: lfAcctDes   *** Function contion accout desc. string    ***
*:                                                                      ***
*:      Documented 01/12/1994                                           ***
*:*************************************************************************
*B600382,1 Reham On 06/06/95
*B600382,1 Make this variable lnOption = 0 to prevent displaying 
*B600382,1 wrong message after coming from GLSLACT screen with <cancel>.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B607235,1 ALB   050/25/2003 Remove Sequance field option from the screen and table
*:**********************************************************************************
*
*** Declaring arrays external to minimize refering errors
*** laData     : containing get fields
*** laKeyField : containing index infomation
*** laScrMode  : containing the current screen mode information
*** laDefProc  : containing the control panel options status (Enable/Disable)
EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc

DECLARE laKeyField [1,4]               && Key field consists of one field
DECLARE laFileStru [1]

laKeyField[1,1] = 'laData[1]'          && The key element position in laData array 
laKeyField[1,2] =.T.                   && This is the last field in the expression
laKeyField[1,3] = 'GRPCODE'            && The Corresponding field elelment 
laKeyField[1,4] = 1                    && This field occupy the first position in the expression

*** Disable some options of the control panel
laDefProc[7]=.F.                       && Delete procedure (lpDelScr )
laDefProc[9]=.F.                       && Save   procedure (lpSaveScr)

*** Variable Declaration

lnGrdSeqno      = 0
lnOldSeqno      = 0.00
lcOldGrID       = ' '
lc_TempDst      = ' '                  && Temporary file for Group Lines
lc_TempTyp      = ' '                  && Temporary file for Account Types
lc_NewAcc       = ' '                  && Temporary file for Selected Accounts
lcFieldStr      = ' '                  && List field string
lcAcctCode      = ' '
lcAccnLdes      = ' ' 
lnGrpLins       = 0                    && Number of Group lines
lnTypeRec       = 0                    && selected account type from list
lnOption        = 0                    && Selected option replace,merge,cancel
lnOldGrLin      = 0                    && Old group lines
lnSelected      = 0
lsActList       = 1                    && List variable (pointer)
cbGrdStat       = 0                    && check box Status

llBrowse        = .F.

*** Program Initialization
IF !gfSetup()    
  RETURN
ENDIF  


SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting=.T.  
  RETURN
ENDIF

*** Selecting header file
SELECT GLGRPHD

*** checking if main window exists
IF !WEXIST(gcBaseWind)
  
  *** Preparing laData by copying empty record to adjust
  *** fields type and width.. 
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK

  *** Create the Name for the temporary files :
  lc_TempDst = gfTempName()            && Temporary file for Group Lines
  lc_TempTyp = gfTempName()            && Temporary file for Account Types
  lc_NewAcc  = gfTempName()            && Temporary file for Selected Accounts
  
  *** Create the temporary file with the appropiate fields
  *** (in this case all the fields of file "GLGRPDT" + 2 more fields
  *** for the record number and status ('M'odify,'A'dd,'D'elete,'S'ame)

  SELECT GLGRPDT
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'NRECNO'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 4
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_TempDst FROM ARRAY laFileStru    

    lcAcctCode  = ALLTRIM(&lc_TempDst..cAcctCode)
  *** Prepare lsFieldStr for the list
    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *lcFieldStr  ="SUBSTR(&lc_TempDst..cAcctCode,1,lnAcsSegSz)+' '+"+;
                 "LOOKUP(GLACCHAR.ctypecode,&lc_TempDst..cAcctCode,"+;
                 "GLACCHAR.cAcctCode,'ACCTCODE')+'  '+lfAcctDes()+'  '+"+;
                 "STR(&lc_TempDst..nGrdSeqno,8)+'  '+IIF(&lc_TempDst..cGrdStat='E','û',' ')"
    lcFieldStr  ="SUBSTR(&lc_TempDst..cAcctCode,1,lnAcsSegSz)+' '+"+;
                 "LOOKUP(GLACCHAR.ctypecode,&lc_TempDst..cAcctCode,"+;
                 "GLACCHAR.cAcctCode,'ACCTCODE')+'  '+lfAcctDes()+'   '+"+;
                 "IIF(&lc_TempDst..cGrdStat='E','û',' ')"
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
    *** indexing the file for further seek
    INDEX ON CACCTCODE TAG ACCTCODE ADDITIVE
    SET ORDER TO TAG AcctCode
ENDIF

*** Selecting header file
SELECT GLGRPHD

*** Calling the main program
*E300683,5 Call *.SPR from screens directory
* DO GLGroup.SPR 
DO (gcScrDir + gcWinAppl + '\GLGroup.SPR')
*E300683,5 end   
*** Clean Up
IF glQuitting
  *** Close open temporary files
  IF USED (lc_TempDst)
    USE IN ALIAS(lc_TempDst)
    ERASE &gcWorkDir.&lc_TempDst..DBF
    ERASE &gcWorkDir.&lc_TempDst..CDX
  ENDIF  
  IF USED (lc_TempTyp)
    USE IN ALIAS(lc_TempTyp)
    ERASE &gcWorkDir.&lc_TempTyp..DBF
    ERASE &gcWorkDir.&lc_TempTyp..CDX
  ENDIF  
  IF USED (lc_NewAcc)
    USE IN ALIAS(lc_NewAcc)
    ERASE &gcWorkDir.&lc_NewAcc..DBF
  ENDIF  
ENDIF 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
* Procdure to control screen modes
PROCEDURE lpShow
lcObjStat=IIF(lnGrpLins<>0,"ENABLE","DISABLE")

DO CASE
  *** Select Mode
  CASE laScrMode[1]
    *** Inotialize Variables for display
    SELECT (lc_TempDst)
    *** Delete old data (if any)
    ZAP

    lnGrpLins   = 0                    && Number of Group lines
    lsActList   = 1                    && List variable (pointer)
    lnGrdSeqno  = 0
    cbGrdStat   = 0                    && check box Included/Excluded
    lcAcctCode  = ' '
    lcAccnLdes  = ' ' 
    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *SHOW GET  lnGrdSeqno               DISABLE
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
    SHOW GET  cbGrdStat                DISABLE
    SHOW GET  lcAcctCode               DISABLE
    SHOW GET  lcAccnLdes               DISABLE
  
  *** View Mode
  CASE laScrMode[2]
    *** Recreating temporary file with actual lines belonging to 
    *** selected header
    SELECT *,RECNO() AS 'nRecNO',"S" AS 'cStatus';
           FROM &gcDataDir.GLGRPDT ;
           INTO DBF &gcWorkDir.&lc_TempDst;
           WHERE cGrpCode = laData[1]
    *** indexing the file for further seek
    INDEX ON CACCTCODE TAG ACCTCODE ADDITIVE
    SET ORDER TO TAG AcctCode
    lcAcctCode  = ALLTRIM(&lc_TempDst..cAcctCode)
    lcAccnLdes  = LOOKUP(GLACCHAR.cAccnLdes,&lc_TempDst..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE')

    lnGrpLins     = RECCOUNT()            && counting selected lines
    lsActList     = 1                     && positioning the pointer to first record

    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *lnGrdSeqno    = nGrdSeqno             && assigning actual value to "nGrdSeqno"
    *cbGrdStat     = IIF(cGrdStat='E',1,0) && convering values to numeric for check box
    *SHOW GET cbGrdStat                   DISABLE
    *SHOW GET lnGrdSeqno                  DISABLE
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
 
  *** Edit Mode
  CASE laScrMode[3]  

    *** Refershing temporary file by recreating it ; for multiuser purposes
    SELECT *,RECNO() AS 'nRecNO',"S" AS 'cStatus';
           FROM &gcDataDir.GLGRPDT ;
           INTO DBF &gcWorkDir.&lc_TempDst;
           WHERE cGrpCode = laData[1]
    *** indexing the file for further seek
    INDEX ON CACCTCODE TAG ACCTCODE ADDITIVE
    *** setting the index order
    SET ORDER TO TAG AcctCode
    lcAcctCode  = ALLTRIM(&lc_TempDst..cAcctCode)
    lcAccnLdes  = LOOKUP(GLACCHAR.cAccnLdes,&lc_TempDst..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE')
    lnGrpLins       = RECCOUNT()             && counting group lines
    lsActList       = 1                      && List variable (pointer)  
    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *lnGrdSeqno      = nGrdSeqno              && refeshing value
    *cbGrdStat       = IIF(cGrdStat='E',1,0)  && getting the numeric value
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
    *** Disableing "lnGrdSeqno & pbRemove" objects if the group has no lines
    lcObjStat       = IIF(RECCOUNT(lc_TempDst)=0,"DISABLE","ENABLE")
    
    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *SHOW GET lnGrdSeqno          &lcObjStat      
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
    
    SHOW GET pbRemove            &lcObjStat      
    
    *** Showing "cbGrdStat" object with proper prompt & status
    SHOW GET cbGrdStat           &lcObjStat
      
  *** Add Mode  
  CASE laScrMode[4]
    *** Showing objects enabled only if the list contains accounts
    *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
    *SHOW GET lnGrdSeqno          &lcObjStat       
    *B607235,1 ALB Remove Sequance field option from the screen and table [End]
    
    SHOW GET cbGrdStat           &lcObjStat    
    SHOW GET pbRemove            &lcObjStat    

ENDCASE 
SHOW GET lcAcctCode
SHOW GET lcAccnLdes

*** Traping any error related to the list when displayed
lcSavError=ON("ERROR")
ON ERROR X=1
SELECT (lc_TempDst) 
SHOW GET lsActList   
*** enabling error handler
ON ERROR &lcSavError
*** selecting header file
SELECT GLGRPHD
 
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
* Validation for key field
FUNCTION lfvData_1
*** eleminating spaces
*** checking that user writes a value and presses enter
IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()= 13)
  IF llBrowse
    =gfSeekRec()
  ELSE
    IF RECCOUNT() <> 1 .AND. RECNO() <= RECCOUNT()
      GOTO RECNO()  
    ENDIF 
    laData[1]=ALLTRIM(laData[1])
    IF LEFT(laData[1],1) <> '?'
      *** if this value is numeric ; zeros is forsed from left side
      *** to complete string length ; for indexing puroses.
      IF ISDIGIT(LEFT(laData[1],1))
        laData[1]=RIGHT("00000000"+ALLTRIM(laData[1]),8) 
      ENDIF 
      *** refershing key field
      SHOW GET laData[1] 
      *** seeking for this key field ; if present : displaying this group
      =gfSeekRec()
    ELSE
      laData[1]='?'
      SHOW GET laData[1] 
      =gfSeekRec()
    ENDIF
  ENDIF
ENDIF
llBrowse = .F.
 
*!**************************************************************************
*!
*!      Function: lfvGrdSeqno
*!
*!**************************************************************************
* Validation for "nGrdSeqno" field

FUNCTION lfvGrdSeqno

SELECT (lc_TempDst)
*** Replacing in the temp. file with the value entered; and adjusting
*** the record status with "M" for Modify except for "A" for Add.
IF lnGrdSeqno >= 0
REPLACE nGrdSeqno WITH lnGrdSeqno,;
          cStatus WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
*** refreshing the list "lsActList" 
SHOW GET lsActList  
*** selecting header file
ELSE
  = gfModalGen("INM02036B00000","Dialog")
  lnGrdSeqno=lnOldSeqno
ENDIF

SELECT glGrpHd

*!**************************************************************************
*!
*!      Function: lfvGrdStat
*!
*!**************************************************************************
* Validation for "cGrdStat" check box

FUNCTION lfvGrdStat
*** selecting temp. file
SELECT (lc_TempDst)
*** in edit and add mode; and while list contains lines ;
*** enabling check box "cbGrdStat"; and replacing in the temp.
*** file with the value entered, and modifing the "cStatus" field
*** with proper flag; "M" for the cases except for "A" (Add) will remain.
IF laScrMode[3] .OR. laScrMode[4] 
  IF lnGrpLins<>0 
    REPLACE cGrdStat WITH IIF(cGrdStat='E','I','E'),;
            cStatus WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
    lcStat="ENABLE"
  ELSE
    lcStat="DISABLE"
  ENDIF
ELSE
  lcStat="DISABLE"
ENDIF
*** showing the check box "cbGrdStat" with proper prompt and status
SHOW GET cbGrdStat                  &lcStat
*** refershing the list
SHOW GET lsActList  
*** selecting the header
SELECT glGrpHd

*!**************************************************************************
*!
*!      Function: lfvAddAct
*!
*!**************************************************************************
* Validation for Add Account option 
FUNCTION lfvAddAct

*** calling subprogram "glSlAct" with 3 paramenters; the 3 temp. files
*** and variable for no of group lines
*** IF llUpdate=.T. ;glSlAct will select accounts criteria and update
*** temp. file "lc_TempDst" otherwise it will select them only
llUpdate=.T.

*** Parameter used to indicate dealing with account file
llAct=.T.

*** lcDataDir will hold data directory path because "glSlAct" will be
*** used as separate module
lcDataDir=gcDataDir
llFlag=.T.

*B600382,1 Reham On 06/06/95
*B600382,1 Make this variable zero to prevent displaying wrong message.
lnOption   = 0

*E300683,5 Call programs from PRGS directory
*DO glSlAct.PRG WITH lc_TempTyp,lc_NewAcc,lc_TempDst,lnGrpLins,'','',llUpdate,llFlag
DO (gcAppHome + gcWinAppl + '\glSlAct.PRG ') WITH lc_TempTyp,lc_NewAcc,lc_TempDst,lnGrpLins,'','',llUpdate,llFlag
*E300683,5 end

*** If there is lines from before

IF llUpdate
  IF lnGrpLins > 0
    DO CASE
      *** Replace option is selected
      CASE lnOption = 1
        *** message with total replaced accounts, corresponding group code
        *** Message : "ð Account added to group ð." ***
        *WAIT ALLTRIM(STR(lnGrpLins))+" Account added to group "+;
              laData[1] WINDOW NOWAIT
        lcTmpStr = ALLTRIM(STR(lnGrpLins)) + "|" + laData[1]
        =gfWait("02241","NOWAIT",lcTmpStr)
      CASE lnOption = 2
        *** message with total merged records, corresponding group code
        *** Message : "ð Account added to group ð." ***
        *WAIT ALLTRIM(STR(lnGrpLins-lnOldGrLin))+" Account added to group "+;
             laData[1] WINDOW NOWAIT
        lcTmpStr = ALLTRIM(STR(lnGrpLins-lnOldGrLin)) + "|" + laData[1]
        =gfWait("02241","NOWAIT",lcTmpStr)
      CASE lnOption = 3
        *** message with total lines added,corresponding group code
        *** Message : "No account added to group ð." ***
        *WAIT " No account added to group "+laData[1] WINDOW NOWAIT
        =gfWait("02242","NOWAIT",laData[1])
      ENDCASE
  ELSE
    IF lnOption =1 .OR. lnOption=2   
      *** message with total replaced accounts, corresponding group code
      *** Message : "ð Account added to group ð." ***
      *WAIT ALLTRIM(STR(lnGrpLins))+" Account added to group "+;
           laData[1] WINDOW NOWAIT
      =gfWait("02241","NOWAIT",laData[1])
    ELSE
      *** message with total lines added,corresponding group code
      *** Message : "No account added to group ð." ***
      *WAIT " No account added to group "+laData[1] WINDOW NOWAIT
      =gfWait("02242","NOWAIT",laData[1])
    ENDIF
  ENDIF
ENDIF
*** enableing some objects when the group contains accounts
*** (added usually from the option add accounts. )
IF lnGrpLins > 0
  *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
  *SHOW GET lnGrdSeqno   ENABLE
  *B607235,1 ALB Remove Sequance field option from the screen and table [End]
  
  SHOW GET cbGrdStat    ENABLE 
  SHOW GET pbRemove     ENABLE
ENDIF

lnOption   = 0
*** addjusting list pointer
lsActList  = 1 
*** returning to the list object
_CUROBJ    = OBJNUM(lsActList)
*** refershing the list
SHOW GET lsActList  
*** selecting the header file
SELECT glGrpHd 

*!**************************************************************************
*!
*!      Function: lfvRemAct
*!
*!**************************************************************************
* Validation for remove account option
FUNCTION lfvRemAct
  *** Confirm removing of the record
  IF gfModalGen("QRM00007B00007","ALERT") = 1
    *** selecting temp. file
    SELECT (lc_TempDst)
    *** if the record is previously modified,"M---->D"
    ***   delete it           
    *** if it is a new entry                 "A---->D"
    ***   skip it when saving 
    *** else (a "Same" record)               "S---->D"
    ***   delete it
    REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)
    *** Delete the current record (to be removed), and skip
    *** for new record, if the removed record is the last one,
    *** go top
    DELETE
    lnGrpLins  =  lnGrpLins - 1
    IF EOF(lc_TempDst)
      GO TOP
    ELSE
      SKIP
    ENDIF

  *** Adjust Controls according to whatever the list contian lines or not
  lcObjStat       = IIF(lnGrpLins=0,"DISABLE","ENABLE")
  *B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
  *SHOW GET lnGrdSeqno &lcObjStat
  *B607235,1 ALB Remove Sequance field option from the screen and table [End]
  
  SHOW GET cbGrdStat  &lcObjStat 
  SHOW GET pbRemove   &lcObjStat

  *** Update list content
  SHOW GET lsActList
  *** refreshing say fields
*  =lfRefresh()
  *** pointing to the list object
  _CUROBJ         = OBJNUM(lsActList) 
  *** selecting header file
  SELECT glGrpHd
  ENDIF

*!**************************************************************************
*!
*!      Function: lfwActList
*!
*!**************************************************************************
* List Validation
FUNCTION lfwActList
*** selecting temp file
SELECT (lc_TempDst)
*** refreshing "nGrdSeqno" value
*B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
*lnGrdSeqno    = nGrdSeqno
*B607235,1 ALB Remove Sequance field option from the screen and table [End]

*** assigning the right numeric value to check box
cbGrdStat     = IIF(cGrdStat='E',1,0)
*** variable will contain "cbGrdStat" check box status
lcStat       = IIF((laScrMode[3] .OR. laScrMode[4]) .AND. lnGrpLins<>0,"ENABLE","DISABLE")

*** showing "cbGrdStat" check box with appropriate prompt & status
SHOW GET cbGrdStat         &lcStat
*** refreshing "nGrdSeqno" field
*B607235,1 ALB Remove Sequance field option from the screen and table [Begin]
*SHOW GET lnGrdSeqno
*B607235,1 ALB Remove Sequance field option from the screen and table [End]

IF !laScrMode[1]
  lcAccnLdes  = LOOKUP(GLACCHAR.cAccnLdes,&lc_TempDst..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE')
  lcAcctCode  = ALLTRIM(&lc_TempDst..cAcctCode)
  SHOW GET lcAcctCode
  SHOW GET lcAccnLdes
ENDIF

*** refreshing say fields
*=lfRefresh()
*** selecting header file
SELECT glGrpHd

*!**************************************************************************
*!
*!      Function: lpSavScr
*!
*!**************************************************************************
* Save group processing 
FUNCTION lpSavScr
*** adding blank record

IF !lnGrpLins > 0
  =gfModalGen("TRM02216B00000","DIALOG")
  llCSave=.F.
  RETURN
ENDIF

IF laScrMode[4] .AND. lnGrpLins > 0
  APPEND BLANK
ENDIF
*** saving header file information
GATHER FROM laData FIELDS &lcScFields
*** saving detail file information
=gfAdd_Info('GLGRPHD')
SELECT (lc_TempDst)
REPLACE ALL &lc_TempDst..cAdd_User WITH gcUser_ID,;
            &lc_TempDst..cAdd_Time WITH gfGetTime(),;
            &lc_TempDst..dAdd_Date WITH DATE(),;
            &lc_TempDst..cLok_User WITH ' ',&lc_TempDst..cLok_Time WITH ' ',;
            &lc_TempDst..dLok_Date WITH {},&lc_TempDst..lLok_Stat WITH .F.
             
=gfTmp2Mast('glGrpDt',lc_TempDst,'Saving Group '+laData[1]+' ...')
*** selecting header file

SELECT glGrpHd

*!**************************************************************************
*!
*!      Function: lpDelScr
*!
*!**************************************************************************
* Delete group processing 
FUNCTION lpDelScr

lnSelected=0
** Selecting temp. file
SELECT GLGRPDT
IF SEEK(laData[1])
  SCATTER MEMVAR BLANK	
  SCAN REST WHILE cGrpCode=laData[1] .AND. lfTherm()
    GATHER MEMVAR 
    DELETE       
  ENDSCAN
ENDIF

IF lnSelected <> lnGrpLins 
  FOR lnSelected=lnSelected TO lnGrpLins 
    =gfThermo(lnGrpLins,lnSelected,'Deleting group..',laData[1])
  ENDFOR
ENDIF

SELECT glGrpHd
** Preparing blank array  
SCATTER FIELDS &lcScFields MEMO TO laData BLANK
** Blanking the record to be deleted
GATHER FROM laData FIELDS &lcScFields
** Now deleting the header record
DELETE


** Returning to select mode
laScrMode       =.F.
laScrMode[1]    =.T.
** Selecting header file
SELECT glGrpHd

*!**************************************************************************
*!
*!      Function: lfAcctDes
*!
*!**************************************************************************
* Function contion accout description s`rhnc
* this function was created to make the list string < 255 character
* it contains the details of account description
FUNCTION lfAcctDes
DO CASE
CASE _DOS
RETURN SUBSTR(LOOKUP(GLACCHAR.cAccnLdes,&lc_TempDst..cAcctCode,;
              GLACCHAR.cAcctCode,'ACCTCODE'),1,53-lnAcsSegSz)
CASE _WINDOWS
RETURN SUBSTR(LOOKUP(GLACCHAR.cAccnLdes,&lc_TempDst..cAcctCode,;
              GLACCHAR.cAcctCode,'ACCTCODE'),1,39-lnAcsSegSz)
ENDCASE

*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
* Function to perform a thermometer for selecting accounts
FUNCTION lfTherm
*** Calling global Function thermometer
lnSelected = lnSelected + 1
=gfThermo(lnGrpLins,lnSelected,'Deleting group..',laData[1])
