*:*************************************************************************
*:                                                                      ***
*: Procedure file: GLCPTYP.PRG                                          ***
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
*B600560,1 Hesham El-Sheltawi 06/30/96
*B600560,1 in copying recaring entries from another compary
*B600560,1 Replace the Recaring entry last gen. date with {}
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
** laWndObj  : Have screen name & first & last obj. for each screen.

DECLARE laSelect[1],laWndObj[2,3],laTypes[4,2]

laSelect [1]   = " "
laTypes  [1,1] = "A"
laTypes  [1,2] = "Allocation"
laTypes  [2,1] = "D"
laTypes  [2,2] = "Distribution"
laTypes  [3,1] = "R"
laTypes  [3,2] = "Recurring"
laTypes  [4,1] = "T"
laTypes  [4,2] = "Template"

DO CASE
  CASE _DOS
    laWndObj [1,1] = "AWRGLCPGRP"
    laWndObj [1,2] = "IBPOPCOM"
    laWndObj [1,3] = "PBCLOSE"
  CASE _WINDOWS
    laWndObj [1,1] = "AWRGLCPGRP"
    laWndObj [1,2] = "PUPOPCOM"
    laWndObj [1,3] = "PBCLOSE"
ENDCASE

laWndObj [2,1] = "CWRGLDETAL"
laWndObj [2,2] = "LSDETAIL"
laWndObj [2,3] = "PBDTCLOSE"

DECLARE laFileStru[1] , laComLst[1]

** laComLst    : Array of companys list.
** lcSGrpDt    :
** lcOldCom    : Var. to store old company id.
** lcComp_ID   : Var. to store new company id.
** lcCompany   : Var. to return selected (company id,name) from popup.
** lcComPath   : Var. to store the path for existing source company.
** lc_SelGrp   : Temp file name for selected accounts.
** lc_Temp1    : Temp file name for group lines.
** lc_Temp2    : Temp file name for selected criteria.
** lcAlTypes   : Alias name to GLTYPES  in source company.
** lcAlAcHar   : Alias name to GLACCHAR in source company.
** lc_SelGrp   :
** lcAlTypes   :
** lcAlAcHar   :
** lcAlAcCod   :
** lcAutHd     :
** lcAutDt     :
** lcTAutDt    :
** lcList_Str  :
** lcAlAcCod   :
** lcAccSeg    :
** lc_Allo     :
** lc_Dist     :
** lc_Recu     :
** lc_Temp     :
** lc_Choise   :
** lc_OldChoise:
** lc_Types    :
** lcGrpCode   :

** lcBut1Stat  :
** lcBut2Stat  :
** laCtrStat   : To disable the browse pad in the menu

** lcPrompt    :
** lcEmptyTxt  :

** lsTypes     :
** puPopcom    :

** lnRecNum    :
** lnAction    :
** lnRecNo     :
** ln_Allo     :
** ln_Dist     :
** ln_Recu     :
** ln_Temp     :
** pbOk        :

** llNoContrl  :

STORE " "      TO lcSGrpDt    , lcOldCom    , lcComp_ID   , lcCompany   ,;
                  lcComPath   , lc_SelGrp   , lc_Temp1    , lc_Temp2    ,;
                  lcAlTypes   , lcAlAcHar   , lc_SelGrp   , lcAlTypes   ,;
                  lcAlAcHar   , lcAlAcCod   , lcAutHd     , lcAutDt     ,;
                  lcTAutDt    , lcList_Str  , lcAlAcCod   , lcAccSeg    ,;
                  lc_Allo     , lc_Dist     , lc_Recu     , lc_Temp     ,;
                  lc_Choise   , lc_OldChoise, lc_Types    , lcGrpCode

STORE "DISABLE" TO lcBut1Stat  , lcBut2Stat  , laCtrStat

lcPrompt    = "\<Select"
lcEmptyTxt  = 'No company Selected'

STORE 1        TO lsTypes     , puPopcom

STORE 0        TO lnRecNum    , lnAction    , lnRecNo     , ln_Allo     ,;
                  ln_Dist     , ln_Recu     , ln_Temp     , pbOk

llNoContrl  = .T.

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
  lcAlAcCod  = gfTempName()  
  lcAutHd    = gfTempName()
  lcAutDt    = gfTempName()  
  lcTAutDt   = gfTempName()  
 

  *** create empty temp file (lc_SelGrp) 
  *** with the same structure of GLTYPES + Inc, CopyStat,cStatus.

  SELECT glAutHd
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CopyStat'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 17
  laFileStru[lnFileStru+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_SelGrp FROM ARRAY laFileStru    

  DO CASE
  CASE _DOS
  lc_Types = "Inc+'    '+cAutType+'     '+cAutCode+'  '+SUBSTR(cAutDes,1,29)+'   '+CopyStat"
  CASE _WINDOWS
  lc_Types = "Inc+'  '+cAutType+'  '+cAutCode+'  '+SUBSTR(cAutDes,1,32)+' '+CopyStat"
  ENDCASE

ENDIF

SELECT (lc_SelGrp)

*E300683,5 Call *.SPR from screens directory
* DO GlCpAut.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpAut.SPR')
*E300683,5 end   
*** case quit this program ...
IF glQuitting 

  *** close temp file (lc_SelGrp) then erase this file. 
  IF USED(lc_SelGrp)
    USE IN ALIAS(lc_SelGrp)
  ENDIF
  ERASE &gcWorkdir.&lc_SelGrp..DBF
  ERASE &gcWorkdir.&lc_SelGrp..CDX

  *** close temp file (lcGrpDt) then erase this file.
  IF USED(lcAutDt)
    USE IN ALIAS(lcAutDt)
  ENDIF

  ERASE &gcWorkdir.&lcAutDt..DBF
  ERASE &gcWorkdir.&lcAutDt..CDX


  IF USED(lcTAutDt)
    USE IN ALIAS(lcTAutDt)
  ENDIF

  ERASE &gcWorkdir.&lcTAutDt..DBF
  ERASE &gcWorkdir.&lcTAutDt..CDX

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
  ln_Allo     = 0
  ln_Dist     = 0
  ln_Recu     = 0
  ln_Temp     = 0
  lc_Allo     = ' '
  lc_Dist     = ' '
  lc_Recu     = ' '
  lc_Temp     = ' '

  lc_Choise   = " "

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

  IF USED(lcAutDt)
    *** if used then select this area 
    SELECT (lcAutDt)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use glGrpDt from the source company in the path lcComPath
  USE &lcComPath.GLAutDt AGAIN ALIAS &lcAutDt ORDER TYPCODACC


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

  IF USED(lcAutHd)
    *** if used then select this area 
    SELECT (lcAutHd)
  ELSE   
    SELECT 0   
  ENDIF
  *** use glGrpHd from the source company in the path lcComPath
  USE &lcComPath.GLAutHd AGAIN ALIAS &lcAutHd ORDER TYPECODE
  
  lnTotalRec = RECCOUNT()

  IF RECNO() <= RECCOUNT()
    GO TOP
  ENDIF  

  IF EOF()
    USE
    SELECT (lc_SelGrp)
    =gfModalGen("TRM02145B00000","Dialog")          
    lcBut1Stat = "DISABLE"
    lcBut2Stat = "DISABLE"
    SHOW GET pbSelOne DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbCopy   DISABLE
    SHOW GET pbSelect DISABLE
    SHOW GET lsTypes  ENABLE
  ELSE
    lcBut1Stat = "ENABLE"  
    SHOW GET pbSelect ENABLE
  ENDIF

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

SELECT (lc_SelGrp)


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
=lfwList()
SHOW GET lsTypes




*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*
FUNCTION  lfvSelAll 
*** check the field status (Inc) for all the file (lc_SelGrp)
*** and change status from unselect to select

lcGrpNo=lsTypes

SELECT (lc_selGrp)
REPLACE ALL &lc_SelGrp..Inc WITH ' û' FOR EMPTY(&lc_selGrp..CopyStat)
*** refresh the list and change prompt according to new status.
lsTypes=lcGrpNo
SHOW GET lsTypes

=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 
*** check the field status (Inc) for all the file (lc_SelGrp)
*** and change status from select   to unselect
lcGrpNo=lsTypes
REPLACE ALL  &lc_SelGrp..Inc WITH '  '
*** refresh the list and change prompt according to new status.
lsTypes=lcGrpNo
=lfwList()
SHOW GET lsTypes

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
lcGrpNo=lsTypes
REPLACE ALL &lc_SelGrp..Inc WITH IIF(Inc=' û','  ',' û') FOR EMPTY(&lc_selGrp..CopyStat)
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

IF RECNO() <= RECCOUNT()
  GO TOP
ENDIF  

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
   =gfModalGen("TRM02144B00000","DIALOG")   
  _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF


SELECT &lcAutDt..*,SPACE(17) AS 'CopyStat',&lcAlAchar..cTypeCode;
  FROM &lcAutDt,&lcAlAchar,&lc_SelGrp;
  WHERE  &lc_SelGrp..cAutType+&lc_SelGrp..cAutCode+&lc_SelGrp..Inc=;
         &lcAutDt..cAutType+&lcAutDt..cAutCode+" û";
  .AND. &lcAutDt..cAcctCode=&lcAlAchar..cAcctCode;
  INTO DBF &gcWorkDir.&lcTAutDt

  INDEX ON cAutType+cAutCode TAG TypCode
  SET ORDER TO TypCode
    

SELECT (lc_SelGrp)

SET RELATION TO cAutType+cAutCode INTO &lcTAutDt ADDITIVE
                                                 
lnTotalRecs=lnTotalRecs+1

SELECT (lc_SelGrp)

ln_Curr = RECNO()

SCAN FOR !EMPTY(&lc_SelGrp..Inc)
  =lfTherm()
  SELECT (lcAutHd)
  IF EOF()
    SELECT (lc_SelGrp)
    REPLACE &lc_SelGrp..CopyStat WITH 'Source deleted',;
    &lc_SelGrp..Inc WITH ' '
  ELSE
    SELECT glAutHd
    *** cheking if the copied type Already exists
    IF SEEK (&lc_SelGrp..cAutType+&lc_SelGrp..cAutCode,"glAutHd")
      SELECT (lc_SelGrp)
      lcAutCode="Automatic entry of Type "+;
      laTypes[ASUBS("laTypes",ASCAN("laTypes",cAutType),1),2];
      +" and code "+Alltrim(&lc_SelGrp..cAutCode)+" "
      lnAction=gfModalGen("TRM02114B02015","Dialog",lcAutCode)      
      DO CASE
        CASE lnAction=1
          IF lfChkDt()
            SELECT glAutHd
            IF gfObj_Lock(.T.)
              DELETE FOR glAutHd.cAutType+glAutHd.cAutCode=;
                         &lc_SelGrp..cAutType+&lc_SelGrp..cAutCode
              SELECT glAutDt
              DELETE FOR glAutDt.cAutType+glAutDt.cAutCode=;
                         &lc_SelGrp..cAutType+&lc_SelGrp..cAutCode

              SELECT (lc_SelGrp)
              IF RECNO() <= RECCOUNT()
                GOTO RECNO()
              ENDIF  
              *** start copy selected group headers
              =lfCopyHd()
              SELECT glAutHd
              =gfObj_Lock(.F.)
            ELSE
              SELECT (lc_SelGrp)
              REPLACE &lc_SelGrp..CopyStat WITH 'Overwrite err.'
            ENDIF  
          ELSE
            SELECT (lc_SelGrp)
            REPLACE &lc_SelGrp..CopyStat WITH 'Copy Error'
          ENDIF
        CASE lnAction=2
          REPLACE &lc_SelGrp..CopyStat WITH 'Already exists'
        CASE lnAction=3
          REPLACE &lc_SelGrp..CopyStat WITH 'Already exists'
          FOR lnSelected=lnSelected TO lnTotalRec
            =gfThermo(lnTotalRec,lnSelected,'Copying Automatic entries..')
          ENDFOR
          EXIT
        ENDCASE
    ELSE
      IF lfChkDt()
        =lfCopyHd()
      ELSE
        SELECT (lc_SelGrp)
        REPLACE &lc_SelGrp..CopyStat WITH 'Copy Error'
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

SELECT (lc_SelGrp)

IF lnRecNum <= RECCOUNT()
  GO lnRecNum
ENDIF  

IF lnAction <> 3
  FOR lnSelected=lnSelected TO lnTotalRec
    =gfThermo(lnTotalRec,lnSelected,'Copying Automatic entries..',' ')
  ENDFOR
ENDIF

SELECT (lc_SelGrp)

REPLACE ALL INC WITH " " FOR !EMPTY(CopyStat)

IF lnRecNum <= RECCOUNT()
  GOTO lnRecNum
ENDIF

=lfwList()

*** refreshing the list
SHOW GET lsTypes  

*** message with no of groups copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Automatic entries copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02234","NOWAIT",ALLTRIM(STR(lnCopyTypes)))

*!**************************************************************************
*!
*!      Function:  lfChkDt
*!
*!**************************************************************************
*

FUNCTION  lfChkDt
llCopyValid=.T.

SELECT (lc_SelGrp)

IF cAutType="A"
  SELECT glAcchar
  IF !SEEK(&lc_SelGrp..cActCode)
    SELECT (lc_SelGrp)
    REPLACE &lc_SelGrp..CopyStat WITH 'Account not found'
    llCopyValid=.F.
  ELSE 
    *** Checking that the selected type range is not overlaping with
    *** any other ranges in the target company file
    SELECT gltypes
    IF SEEK(glAcchar.cTypeCode)
      LOCATE FOR BETWEEN( VAL(&lc_SelGrp..cActCode) , VAL(glTypes.cTypLacno) , VAL(glTypes.cTypUacno) ) 
      *** checking that the selected type is not a primary one
      *** otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lc_SelGrp)
        REPLACE &lc_SelGrp..CopyStat WITH 'Type Mismatch'
        llCopyValid=.F.
      ENDIF
    ELSE
      SELECT (lc_SelGrp)
      REPLACE &lc_SelGrp..CopyStat WITH 'Type not found'
      llCopyValid=.F.
    ENDIF  
  ENDIF      
ENDIF

SELECT (lcTAutDt)
lnDtNo=RECNO()

SCAN REST WHILE &lcTAutDt..cAutType+&lcTAutDt..cAutCode=;
                &lc_SelGrp..cAutType+&lc_SelGrp..cAutCode  
  SELECT glAcchar
  IF !SEEK(&lcTAutDt..cAcctCode)
    SELECT (lcTAutDt)
    REPLACE &lcTAutDt..CopyStat WITH 'Account not found'
    llCopyValid=.F.
    LOOP
  ELSE 
    *** Checking that the selected type range is not overlaping with
    *** any other ranges in the target company file
    SELECT gltypes
    IF SEEK(&lcTAutDt..cTypeCode)
      LOCATE FOR BETWEEN( VAL(&lcTAutDt..cAcctCode) , VAL(glTypes.cTypLacno) , VAL(glTypes.cTypUacno) ) 
      *** checking that the selected type is not a primary one
      *** otherwise bypassing range checking.
      IF !FOUND()
        SELECT (lcTAutDt)
        REPLACE &lcTAutDt..CopyStat WITH 'Type Mismatch'
        llCopyValid=.F.
        LOOP
      ENDIF
    ELSE
      SELECT (lcTAutDt)
      REPLACE &lcTAutDt..CopyStat WITH 'Type not found'
      llCopyValid=.F.
      LOOP
    ENDIF  
  ENDIF      
ENDSCAN 

SELECT (lcTAutDt)

IF lnDtNo <= RECCOUNT()
  GO lnDtNo
ENDIF  
  


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
INSERT INTO &gcDataDir.glAutHd FROM MEMVAR
*B600560,1 Hesham El-Sheltawi (Start)
*B600560,1 Replace the Recaring entry last gen. date with {}
REPLACE glAutHd.DautlgDat WITH {}
*B600560,1 Hesham El-Sheltawi (End)
*** adding audit information to "gltypes" files
SELECT glAutHd
=gfObj_Lock(.T.)
=gfAdd_Info("glAutHd")

*** adjusting copied groups counter 
lnCopyTypes = lnCopyTypes + 1

SELECT (lcTAutDt)
ln_DtNo=RECNO()

SCAN REST WHILE &lcTAutDt..cAutType+&lcTAutDt..cAutCode =;
                &lc_SelGrp..cAutType+&lc_SelGrp..cAutCode  
  *** Update gltypes
  *** collecting groups from temporary file to memory variables
  SCATTER MEMVAR
  SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
  *** inserting selected groups into "gltypes" file
  INSERT INTO &gcDataDir.glAutDt FROM MEMVAR
  *** adding audit information to "gltypes" files
  =gfAdd_Info("glAutDt")


ENDSCAN

SELECT (lcTAutDt)

IF ln_DtNo <= RECCOUNT()
  GOTO ln_DtNo
ENDIF  

*** replacing copystat with proper variable 
SELECT (lc_SelGrp)
REPLACE &lc_SelGrp..CopyStat WITH 'Copied',;
        &lc_SelGrp..Inc WITH ' ' 

SELECT glAutHd
=gfObj_Lock(.F.)

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
*!      Function:  lfwSelect
*!
*!**************************************************************************
*

FUNCTION lfwSelect

laSelect = ' '
DIMENSION laSelect  [1]

SELECT cAutType+cAutCode FROM &lc_SelGrp;
 WHERE Inc=" û";
 INTO ARRAY laSelect

*!**************************************************************************
*!					
*!      Function:  lfvSelect
*!
*!**************************************************************************
*

FUNCTION lfvSelect
*E300683,5 Call *.SPR from screens directory
* DO GLCHTYP.SPR 
DO (gcScrDir + gcWinAppl + '\GLCHTYP.SPR')
*E300683,5 end   
IF pbOk=1 .AND. lc_OldChoise <> lc_Choise
   lnSelected  = 0
   SELECT (lc_SelGrp)
   SET RELATION TO
   SET ORDER TO
   lsTypes   = 1     
   SELECT &lcAutHd..*,'   ' AS 'Inc',SPACE(17) AS 'CopyStat';
   FROM &lcAutHd;
   WHERE cAutType $ lc_Choise;
   INTO DBF &gcWorkDir.&lc_SelGrp
   SELECT (lc_SelGrp)  
   INDEX ON cAutType+cAutCode TAG cTypeCode         
   SET ORDER TO TAG cTypeCode         
   SET RELATION TO cAutType+cAutCode INTO &lcAutHd ADDITIVE
   SELECT (lc_SelGrp)  
   lnStat=1   
   SCAN 
     lnStat=ASCAN("laSelect",cAutType+cAutCode)
     IF lnStat <> 0
       REPLACE INC WITH " û"
     ENDIF
   ENDSCAN

  IF RECNO() <= RECCOUNT() 
    GO TOP
  ENDIF  
  
  lsTypes   = 1     
  SHOW GET lsTypes  
  lcBut1Stat = "ENABLE"
  lcBut2Stat = "ENABLE"
  SHOW GET pbSelOne ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbCopy   ENABLE
  =lfwList()
ENDIF
pbOk=0
SELECT (lc_SelGrp)

*!**************************************************************************
*!					
*!      Function:  lfvAllo
*!
*!**************************************************************************
*

FUNCTION lfvAllo
lc_Allo=IIF(ln_allo=1,"A"," ")

*!**************************************************************************
*!					
*!      Function:  lfvDist
*!
*!**************************************************************************
*

FUNCTION lfvDist
lc_Dist=IIF(ln_Dist=1,"D"," ")

*!**************************************************************************
*!					
*!      Function:  lfvRecu
*!
*!**************************************************************************
*

FUNCTION lfvRecu
lc_Recu=IIF(ln_Recu=1,"R"," ")

*!**************************************************************************
*!					
*!      Function:  lfvTemp
*!
*!**************************************************************************
*

FUNCTION lfvTemp
lc_Temp=IIF(ln_Temp=1,"T"," ")


*!**************************************************************************
*!
*!      Function:  lfvSlClose
*!
*!**************************************************************************
*
FUNCTION  lfvSlClose

lc_OldChoise=lc_Choise
lc_Choise   =lc_allo+lc_Dist+lc_Recu+lc_Temp

IF EMPTY(lc_Choise)
  PbOk=0
*  WAIT "You have to select at least one type ..." WINDOW NOWAIT
  =gfWait("02235","NOWAIT")
  RETURN .F.
ELSE
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*

FUNCTION  lfvClose
*** close files and quitting

glQuitting  = .T.  
IF USED(lcAlTypes)
  USE IN ALIAS(lcAlTypes)
ENDIF
IF USED(lcAutDt)
  USE IN ALIAS(lcAutDt)
ENDIF

IF USED(lcAutHd)
  USE IN ALIAS(lcAutHd)
ENDIF
IF USED(lcAlAcHar)
  USE IN ALIAS(lcAlAcHar)
ENDIF


*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
* Function to perform a thermometer for selecting accounts
FUNCTION lfTherm
*** Calling global Function thermometer
lnSelected = lnSelected + 1
=gfThermo(lnTotalRec,lnSelected,'Copying automatic entries..',&lc_SelGrp..cAutCode)
