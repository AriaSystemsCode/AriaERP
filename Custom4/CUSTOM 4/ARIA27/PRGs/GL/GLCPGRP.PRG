*:************************************************************************
*:                                                                      ***
*: Procedure file: GLCPGRP.PRG                                          ***
*:                                                                      ***
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Wadie                                           ***
*:      Copyright (c)                                                   ***
*:  Last modified: 01/09/1994                                           ***
*:                                                                      ***
*:  Procs & Fncts:                                                      ***
*:                                                                      ***
*:    FUNCTION  lfwPopup    ***  When popup function                    ***
*:    FUNCTION  lfvPopup    ***  Valid popup function                   ***
*:    FUNCTION  lfwList     ***  When list function                     ***
*:    FUNCTION  lfvSelOne   ***  Function to select one acounts         ***
*:    FUNCTION  lfvSelAll   ***  Function to select all accounts        ***
*:    FUNCTION  lfvSelNon   ***  Function to unselect all accounts      ***
*:    FUNCTION  lfvInvert   ***  Function to invert select status       ***
*:    FUNCTION  lfvCopy     ***  Function to copy selected accounts     ***
*:    FUNCTION  lfCheckTyp  ***  To check type,range                    ***
*:    FUNCTION  lfvPrint    ***  Function to print copy stat.report     ***
*:    FUNCTION  lfvSelTyp   ***  Function to select type to copy        ***
*:    FUNCTION  lfvClose    ***  Close                                  ***
*:    FUNCTION  lfSamType   ***  To check if same type structure        ***
*:                                                                      ***
*:      Documented 01/09/1994                                           ***
*:*************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*
DECLARE laWndObj  [2,3]    && Have screen name & first & last obj. for each screen.

DECLARE laComLst  [1]  && array of companys list
DECLARE laFileStru[1]

llNoContrl  = .T.

lcSGrpDt    = " "
lcOldCom    = " "    && var. to store old company id     
lcComp_ID   = " "    && var. to store new company id  
lcCompany   = " "    && var. to return selected (company id,name) from popup
lcComPath   = " "    && var. to store the path for existing source company
lc_SelGrp   = " "    && Temp file name for selected accounts
lc_Temp1    = " "    && Temp file name for group lines
lc_Temp2    = " "    && Temp file name for selected criteria
lcAlTypes   = " "    && Alias name to GLTYPES  in source company
lcAlAcHar   = " "    && Alias name to GLACCHAR in source company
lnNoOfAcc   = 0      && var. to store number of selected accounts
lnRecNum    = 0 
lnAction    = 0 
lnRecNo     = 0 
lc_Types    = " "
lcGrpCode   = " "
lcBut1Stat  = "DISABLE"
lcBut2Stat  = "DISABLE"
lc_Title    = "Group accounts"
lcPrompt    = "\<Select"
lsTypes     = 1
puPopcom    = 1
lcList_Str  = " "
lcAccSeg    = " "
lc_SelGrp   = " "
lcAlTypes   = " "
lcAlAcHar   = " "
lcGrpHd     = " "
lcSGrPDt    = " "
lcVGrpDt    = " "
lcGrpDt     = " " 
lcEmptyTxt  = 'No company Selected'
laCtrStat   = 'DISABLE' && To disable the browse pad in the menu

LSDETAIL    = ' '

*** Program Initialization
IF !gfSetup()    
  RETURN
ENDIF  

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE

IF EOF()
  * The chart of accounts is empty. You have to create
  * the chart of accounts first.
  * < Ok > 
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF


DO CASE
  CASE _DOS
    laWndObj [1,1] = gcBaseWind
    laWndObj [1,2] = "IBPOPCOM"
    laWndObj [1,3] = "PBCLOSE"
  CASE _WINDOWS
    laWndObj [1,1] = gcBaseWind
    laWndObj [1,2] = "PUPOPCOM"
    laWndObj [1,3] = "PBCLOSE"
ENDCASE

laWndObj [2,1] = "CWRGLDETAL"
laWndObj [2,2] = "LSDETAIL"
laWndObj [2,3] = "PBDTCLOSE"

IF !WEXIST(gcBaseWind)

  *** Func. in the GL.PRG to see if there is other companies ***
  *** have the same account code strucure.

  IF !lfSamAcode()
    glQuitting=.T.  
    RETURN
  ENDIF

  *** redimension array laComLst (increase the number of rows by one)
  *** and insert a new row in array laComLst with null values.
  DIMENSION laComLst[ALEN(laComLst,1)+1,2]
  =AINS(laComLst,1)
  lcCompany     = lcEmptyTxt
  laComLst[1,1] = lcEmptyTxt
  laComLst[1,2] = ' '

  *** Create the Name for the temporary file
  lc_SelGrp = gfTempName()
  lcAlTypes  = gfTempName()
  lcAlAcHar  = gfTempName()
  lcGrpHd    = gfTempName()
  lcSGrPDt   = gfTempName()  
  lcVGrpDt   = gfTempName()  
  lcGrpDt    = gfTempName()

  SELECT glAcchar   
  *** use glGrpDt from the source company in the path lcComPath
  COPY STRUCTURE TO &gcWorkDir.&lcAlAcHar
  SELECT 0
  USE &gcWorkDir.&lcAlAcHar
    
  SELECT GLGrpDt  
  *** use glGrpDt from the source company in the path lcComPath
  COPY STRUCTURE TO &gcWorkDir.&lcSGrpDt
  SELECT 0 
  USE &gcWorkDir.&lcSGrpDt
  

  *** create empty temp file (lc_SelGrp) 
  *** with the same structure of GLTYPES + Inc, CopyStat,cStatus.

  SELECT glGrpHd
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CopyStat'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 14
  laFileStru[lnFileStru+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_SelGrp FROM ARRAY laFileStru    

  SELECT (lc_SelGrp)
  INDEX ON cGrpCode TAG CGRPCODE         
         
  DO CASE
  CASE _DOS
  lc_Types = "Inc+'   '+cGrpCode+'     '+SUBSTR(cGrpLnHed,1,33)+'   '+CopyStat"
  lcList_Str = "SUBSTR(&lcSGrpDt..cAcctCode,1,lnAcsSegSz)+'   '+"+;
               "&lcAlAcHar..caccnldes"

  CASE _WINDOWS
  lc_Types = "Inc+' '+cGrpCode+' '+SUBSTR(cGrpLnHed,1,38)+' '+CopyStat"
  lcList_Str = "SUBSTR(&lcSGrpDt..cAcctCode,1,lnAcsSegSz)+'   '+"+;
               "&lcAlAcHar..caccnldes"
  ENDCASE
  
 
ENDIF

SELECT (lc_SelGrp)

*E300683,5 Call *.SPR from screens directory
* DO GlCpGrp.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpGrp.SPR')
*E300683,5 end   

SELECT (lcSGrpDt)
SET RELATION TO
SET FILTER TO

*** case quit this program ...
IF glQuitting 
  IF WEXIST("CWRGLDETAL")
    RELEASE WINDOW CWRGLDETAL
  ENDIF
  
  IF USED(lcAlTypes)
    SELECT(lcAlTypes)
    USE 
  ENDIF

  IF USED(lcSGrpDt)
    SELECT(lcSGrpDt)
    USE
  ENDIF

  IF USED(lcGrpHd)
    SELECT(lcGrpHd)
    USE
  ENDIF

  IF USED(lcAlAcHar)
    SELECT(lcAlAcHar)
    USE
  ENDIF

  *******************
  *** close temp file (lc_SelGrp) then erase this file. 
  IF USED(lc_SelGrp)
    SELECT(lc_SelGrp)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_SelGrp..DBF
  ERASE &gcWorkdir.&lc_SelGrp..CDX

  *** close temp file (lcGrpDt) then erase this file.
  IF USED(lcGrpDt)
    SELECT(lcGrpDt)
    USE
  ENDIF

  ERASE &gcWorkdir.&lcGrpDt..DBF
  ERASE &gcWorkdir.&lcGrpDt..CDX

  IF USED(lcVGrpDt)
    SELECT(lcGrpDt)
    USE
  ENDIF 

  ERASE &gcWorkdir.&lcVGrpDt..DBF
  ERASE &gcWorkdir.&lcVGrpDt..CDX

ENDIF

*!**************************************************************************
*!
*!      Function:  lfwPopup
*!
*!**************************************************************************
*
FUNCTION  lfwPopup
*** returns 2 characters from lcCompany that contains 
*** company_id + ' ' + company_name
DO CASE
  CASE _DOS
    lcOldCom = LEFT(lcCompany,2)
    lcColor1 = SCHEME(1,6)
    lcColor2 = SCHEME(1,2)
    *** Show get all hand made popups with the appropirate color. ***
    SHOW GET ibPopCom  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  CASE _WINDOWS
    lcOldCom  = LEFT(laComLst[puPopCom,1],2)
ENDCASE

*!**************************************************************************
*!
*!      Function:  lfvPopup
*!
*!**************************************************************************
*

FUNCTION  lfvPopup
*** calling (gfActPop)  to activate the popup to change the source 
*** company from the list popup menu and return the path for selected 
*** company and returns 2 characters from lcCompany that contains 
*** company_id + ' ' + company_name

DO CASE
  CASE _DOS
    lcComPath  = ALLTRIM(gfActPop(1,3,IIF(ALEN(laComLst,1) > 8,9;
    ,ALEN(laComLst,1)+3),35,'laComLst',2,1,@lcCompany))  
    lcComp_ID  = LEFT(lcCompany,2)
    lcColor1 = SCHEME(1,6)
    lcColor2 = SCHEME(1,2)
    *** Show get all hand made popups with the appropirate color. ***
    SHOW GET ibPopCom  COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
  CASE _WINDOWS
    lcComPath  = ALLTRIM(laComLst[puPopCom,2])
    lcComp_ID  = LEFT(laComLst[puPopCom,1],2)
ENDCASE


*** check if the user change the source comapny
IF lcComp_ID <> lcOldCom

  IF WVISIBLE("CWRGLDETAL")
   =gfchclose("CWRGLDETAL")
  ENDIF


  =lfRefresh()
  *** check if the temp file (lcAlTypes) is used before

  IF USED(lcAlTypes)
    *** if used then select this area 
    SELECT (lcAlTypes)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use GLTypes from the source company in the path lcComPath
  *** and set index to order tag TYPECODE
  USE &lcComPath.GLTypes  AGAIN ALIAS  &lcAlTypes ORDER TAG TYPECODE

  IF USED(lcSGrpDt)
    *** if used then select this area 
    SELECT (lcSGrpDt)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use glGrpDt from the source company in the path lcComPath
  USE &lcComPath.GLGrpDt AGAIN ALIAS &lcSGrpDt ORDER TAG GRCODACC


  IF USED(lcAlAcHar)
    *** if used then select this area 
    SELECT (lcAlAcHar)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use glGrpDt from the source company in the path lcComPath
  USE &lcComPath.glAcchar AGAIN ALIAS &lcAlAcHar ORDER TAG ACCTCODE


  *** check if the the first row in the array contains empty
  *** in case if this is the first time that change active company.
  IF EMPTY(laComLst[1,2])
     *** delete the first raw in the array and redimension the array.
     =ADEL(laComLst,1)
     DIMENSION laComLst[Alen(lacomlst,1)-1,2]
     DO CASE
       CASE _WINDOWS
         puPopCom=puPopCom-1
         SHOW GET puPopCom ENABLE
     ENDCASE
  ELSE
    *** select (lc_SelGrp) and removes all records.
    *** and refresh the list window
    SELECT (lc_SelGrp)
    lsTypes   = 1     
    SHOW GET lstypes  
    ZAP       
    SHOW GET lsTypes 
  ENDIF

*E300692,1 Change file name from sycaccod to accod
  *SELECT SYCACCOD
  SELECT ACCOD
*E300692,1 end
*E300789,7 [BEGIN] TAG COMPID WAS DELETED MAKE THE REQUIRED MODIFICATIONS
*  SET ORDER TO TAG COMPID
*  IF SEEK(lcComp_ID)
*    lcAccSeg = cAcSegDes
*  ELSE
*    lcAccSeg = ""
*  ENDIF
  IF !EOF()
    lcAccSeg = cAcSegDes
  ELSE
    lcAccSeg = ""
  ENDIF
*E300789,7 [END..]  
  IF USED(lcGrpHd)
    *** if used then select this area 
    SELECT (lcGrpHd)
  ELSE   
    SELECT 0   
  ENDIF
  *** use glGrpHd from the source company in the path lcComPath
  USE &lcComPath.GLGrpHd AGAIN ALIAS &lcGrpHd ORDER TAG GRPCODE

  lnTotalRec = RECCOUNT()

  IF RECNO() <= RECCOUNT()
    GO TOP
  ENDIF
    
  IF EOF()
    USE
    SELECT (lc_SelGrp)
    =gfModalGen("TRM02119B00000","Dialog","group")          
    lcBut1Stat = "ENABLE"
    lcBut2Stat = "DISABLE"
    SHOW GET pbView   DISABLE
    SHOW GET pbSelOne DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbCopy   DISABLE
    SHOW GET lsTypes  ENABLE
    RETURN
  ENDIF

  lnSelected  = 0
 
  SELECT *,'   ' AS 'Inc' ,SPACE(14) AS 'CopyStat';
   FROM &lcGrpHd;
   INTO DBF &gcWorkDir.&lc_SelGrp
   
   INDEX ON cGrpCode TAG CGRPCODE         
   SET ORDER TO TAG CGRPCODE         
     
   lsTypes   = 1     
   SHOW GET lsTypes  
  
   SELECT(lc_SelGrp)
   SET RELATION TO cGrpCode INTO &lcGrpHd ADDITIVE

  SELECT (lc_SelGrp)
  lcBut2Stat = "ENABLE"
  SHOW GET lsTypes  ENABLE
  SHOW GET pbView   ENABLE
  SHOW GET pbSelOne ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbCopy   ENABLE

ENDIF


*!**************************************************************************
*!
*!      Function:  lfwList
*!
*!**************************************************************************
*

FUNCTION  lfwList

*** check if the number of accounts in the list greater than 1 
SELECT (lc_SelGrp)
*lnRecNo=RECNO()

IF RECCOUNT() > 0  
  IF EMPTY(&lc_SelGrp..CopyStat) 
    *** check the status for (Inc) field 
    IF !EMPTY(&lc_SelGrp..Inc)
      *** case the selected .. change the prompt to 'unselect'
      lcPrompt = "\<Unselect"
      SHOW GET pbSelOne,1 PROMPT '\<Unselect'      ENABLE
    ELSE
      *** case the unselected .. change the prompt to 'select'
      lcPrompt = "\<Select"
      SHOW GET pbSelOne,1 PROMPT '\<Select'        ENABLE
    ENDIF
  ELSE
      lcPrompt = "Select"
      SHOW GET pbSelOne,1 PROMPT 'Select'          DISABLE
  ENDIF
ENDIF

IF WVISIBLE("CWRGLDETAL")
  SELECT (lcSGrpDt)
  SET RELATION TO
  SET FILTER TO &lcSGrpDt..cGrpCode+&lcSGrpDt..cAcctCode = &lc_SelGrp..cGrpCode
  SET RELATION TO &lcSGrpDt..cAcctCode INTO &lcAlAcHar ADDITIVE
  SHOW GETS WINDOW CWRGLDETAL
  SELECT (lc_SelGrp)
ENDIF

SELECT (lc_SelGrp)
*GOTO lnRecNo


*!**************************************************************************
*!					
*!      Function:  lfvSelOne 
*!
*!**************************************************************************
*
FUNCTION  lfvSelOne 
*** check the status for the field (Inc) 
*** and change status from select   to unselect
***                or from unselect to select
lcGrpNo=lsTypes
SELECT (lc_selGrp)
IF EMPTY(&lc_selGrp..CopyStat)
  REPLACE  &lc_SelGrp..Inc WITH IIF(Inc=' û','  ',' û')
ENDIF
*** refresh the list and change prompt according to new status.
lsTypes=lcGrpNo

SHOW GET lsTypes

=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*
FUNCTION  lfvSelAll 
*** check the field status (Inc) for all the file (lc_SelGrp)
*** and change status from unselect to select
SELECT (lc_SelGrp)
lcGrpNo=lsTypes
REPLACE &lc_SelGrp..Inc WITH ' û' FOR EMPTY(&lc_selGrp..CopyStat)
*** refresh the list and change prompt according to new status.
SHOW GET lsTypes
=lfwList()
lsTypes=lcGrpNo

*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 
*** check the field status (Inc) for all the file (lc_SelGrp)
*** and change status from select   to unselect
SELECT (lc_SelGrp)
lcGrpNo=lsTypes
REPLACE ALL  &lc_SelGrp..Inc WITH '  '
*** refresh the list and change prompt according to new status.
SHOW GET lsTypes
=lfwList()
lsTypes=lcGrpNo

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 

*** check the status field (Inc) for all the file (lc_SelGrp)
*** and change status from select   to unselect
***                or from unselect to select

SELECT (lc_SelGrp)
lcGrpNo=lsTypes
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),IIF(Inc=' û','  ',' û'),'')
*** refresh the list and change prompt according to new status.
SHOW GET lsTypes
=lfwList()
lsTypes=lcGrpNo




*!**************************************************************************
*!
*!      Function:  lfvCopy 
*!
*!**************************************************************************
*

*** function validate that the copied group is not existing in the 
*** target file and group accounts found in the chart of the target
*** company and each account type ranges does not overlap with type  
*** ranges in the target company
FUNCTION  lfvCopy

lnTotRecs=0
SELECT (lc_SelGrp)
lnRecNum=RECNO()

*** count the number of included groups to the variable lnTotRecs
COUNT FOR !EMPTY(Inc) TO lnTotalRec

*** initialize no. of copied groups , current type variable.
lnCopyTypes = 0               && init. the no of copied groups.
lnCurRec    = 0               && init. the current type variable.
lnSelected  = 0
*** check if there is any selected groups to copied
IF lnTotalRecs = 0
   *** case there is no selected groups to copyied
   *** Message "No included groups to copy."
   *** and change the cureent object to (Select) 
   *** then exit from this function.
   =gfModalGen("TRM02115B00000","DIALOG")   
  _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF
SELECT (lcSGrpDt)
SET ORDER TO TAG ACCTCODE         

SELECT &lcSGrpDt..*,SPACE(16) AS 'CopyStat',&lcAlAchar..cTypeCode;
  FROM &lcSGrpDt,&lcAlAchar,&lc_SelGrp;
  WHERE  &lc_SelGrp..cGrpCode+&lc_SelGrp..Inc=;
         &lcSGrpDt..cGrpCode+" û";
  .AND. &lcSGrpDt..cAcctCode=&lcAlAchar..cAcctCode;
  INTO DBF &gcWorkDir.&lcGrpDt

  INDEX ON cGrpCode TAG GrpCode
  SET ORDER TO GrpCode
    
SELECT (lcSGrpDt)
SET ORDER TO GRCODACC

SELECT (lc_SelGrp)
INDEX ON Inc TAG Inc         
SET ORDER TO TAG Inc
SET RELATION TO cGrpCode INTO &lcGrpDt ADDITIVE

lnTotalRecs=lnTotalRecs+1

SCAN FOR !EMPTY(Inc)
  =lfTherm()
  SELECT (lcGrpHd)
  IF EOF()
    SELECT (lc_SelGrp)
    REPLACE &lc_SelGrp..CopyStat WITH 'Source deleted',;
    &lc_SelGrp..Inc WITH ' '
  ELSE
    SELECT glGrpHd
    *** cheking if the copied type Already exists
    IF SEEK (&lc_SelGrp..cGrpCode,"glGrpHd")
      SELECT (lc_SelGrp)
      lcGrpCode="Group "+&lc_SelGrp..cGrpCode
      lnAction=gfModalGen("TRM02114B02015","Dialog",lcGrpCode)      
      DO CASE
        CASE lnAction=1
          IF lfChkDt()
            SELECT glGrpHd
            IF gfObj_Lock(.T.)
              DELETE FOR glGrpHd.cGrpCode=&lc_SelGrp..cGrpCode
              SELECT glGrpDt
              DELETE FOR glGrpDt.cGrpCode=&lc_SelGrp..cGrpCode
              SELECT (lc_SelGrp)
              *** start copy selected group headers
              =lfCopyHd()
            ELSE
              SELECT (lc_SelGrp)
              REPLACE &lc_SelGrp..CopyStat WITH  'Overwrite err.'
              REPLACE &lc_SelGrp..Inc      WITH  ' '
            ENDIF  
          ELSE
            SELECT (lc_SelGrp)
            REPLACE &lc_SelGrp..CopyStat WITH 'Copy Error',;
            &lc_SelGrp..Inc WITH ' '
          ENDIF
        CASE lnAction=2
          REPLACE &lc_SelGrp..CopyStat WITH 'Already exists',;
          &lc_SelGrp..Inc WITH ' '
        CASE lnAction=3
          REPLACE &lc_SelGrp..CopyStat WITH 'Already exists',;
          &lc_SelGrp..Inc WITH ' '
          EXIT
        ENDCASE
    ELSE
      IF lfChkDt()
        =lfCopyHd()
      ELSE
        SELECT (lc_SelGrp)
        REPLACE &lc_SelGrp..CopyStat WITH 'Copy Error',;
        &lc_SelGrp..Inc WITH ' '
      ENDIF
    ENDIF
  ENDIF
  SELECT (lc_SelGrp)
ENDSCAN
FOR lnSelected=lnSelected TO lnTotalRec
  =gfThermo(lnTotalRec,lnSelected,'Copying groups..',' ')
ENDFOR
SELECT (lc_SelGrp)
SET ORDER TO TAG CGRPCODE         

IF lnRecNum <= RECCOUNT()
  GOTO lnRecNum
ENDIF  

*** refreshing the list
SHOW GET lsTypes  
=lfwList()

*** message with no of groups copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Groups copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02237","NOWAIT",ALLTRIM(STR(lnCopyTypes)))

*!**************************************************************************
*!
*!      Function:  lfChkDt
*!
*!**************************************************************************
*

FUNCTION  lfChkDt

llCopyValid=.T.

SELECT (lcGrpDt)

lnDtNo=RECNO()

SCAN REST WHILE &lcGrpDt..cGrpCode=&lc_SelGrp..cGrpCode  

  SELECT glAcchar
  IF !SEEK(&lcGrpDt..cAcctCode)
    SELECT (lcGrpDt)
    REPLACE &lcGrpDt..CopyStat WITH 'Account not found'
    llCopyValid=.F.
    LOOP
  ELSE 
    *** Checking that the selected type range is not overlaping with
    *** any other ranges in the target company file
    SELECT gltypes
    IF SEEK(&lcGrpDt..cTypeCode)
      LOCATE FOR BETWEEN( VAL(&lcGrpDt..cAcctCode) , VAL(glTypes.cTypLacno) , VAL(glTypes.cTypUacno) ) 
      *** checking that the selected type is not a primary one
      *** otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lcGrpDt)
        REPLACE &lcGrpDt..CopyStat WITH 'Type Mismatch'
        llCopyValid=.F.
        LOOP
      ENDIF
    ELSE
      SELECT (lcGrpDt)
      REPLACE &lcGrpDt..CopyStat WITH 'Type not found'
      llCopyValid=.F.
      LOOP
    ENDIF  
  ENDIF      
  SELECT (lcGrpDt)
ENDSCAN 

SELECT (lcGrpDt)

IF RECCOUNT() <> 0
  GOTO lnDtNo
ENDIF

SELECT (lc_SelGrp)

RETURN llCopyValid==.T.

*!**************************************************************************
*!
*!      Function:  lfCopyHd
*!
*!**************************************************************************
*
FUNCTION  lfCopyHd

SELECT (lc_SelGrp)
*** Update gltypes
*** collecting groups from temporary file to memory variables
SCATTER MEMVAR
SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
*** inserting selected groups into "gltypes" file
INSERT INTO &gcDataDir.glGrpHd FROM MEMVAR
*** adding audit information to "gltypes" files

SELECT GLGRPHD
=gfObj_Lock(.T.)
=gfAdd_Info("GLGRPHD")

*** adjusting copied groups counter 
lnCopyTypes = lnCopyTypes + 1

SELECT (lcGrpDt)

SCAN REST WHILE &lcGrpDt..cGrpCode=&lc_SelGrp..cGrpCode  
  *** Update gltypes
  *** collecting groups from temporary file to memory variables
  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  *** inserting selected groups into "gltypes" file
  INSERT INTO &gcDataDir.glGrpDt FROM MEMVAR
  *** adding audit information to "gltypes" files
  =gfAdd_Info("glGrpDt")
ENDSCAN

SELECT GLGRPHD
=gfObj_Lock(.F.)

*** replacing copystat with proper variable 
SELECT (lc_SelGrp)
REPLACE &lc_SelGrp..CopyStat WITH 'Copied',;
        &lc_SelGrp..Inc WITH ' ' 


*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function
WAIT "Leave it" WINDOW NOWAIT

*!**************************************************************************
*!					
*!      Function:  lfvView
*!
*!**************************************************************************
*

FUNCTION lfvView
=gfActWind('CWRGLDETAL',lc_Title)
SHOW GET lcAccSeg 

SELECT (lcSGrpDt)

SET RELATION TO
SET FILTER TO &lcSGrpDt..cGrpCode+&lcSGrpDt..cAcctCode = &lc_SelGrp..cGrpCode
SET RELATION TO &lcSGrpDt..cAcctCode INTO &lcAlAcHar ADDITIVE
SHOW GETS WINDOW CWRGLDETAL
SELECT (lc_SelGrp)

*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*

FUNCTION  lfvClose
*** close files and quitting
glQuitting  = .T.  


*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
* Function to perform a thermometer for selecting accounts
FUNCTION lfTherm
*** Calling global Function thermometer
lnSelected = lnSelected + 1
=gfThermo(lnTotalRec,lnSelected,'Copying groups..',&lc_SelGrp..cGrpCode)
