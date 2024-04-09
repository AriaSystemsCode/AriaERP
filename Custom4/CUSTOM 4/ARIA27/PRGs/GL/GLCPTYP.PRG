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
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300789,7 WALID 03/08/99 Remove cComp_id from files
DECLARE laComLst  [1]  && array of companys list
DECLARE laFileStru[1]

lcOldCom    = " "    && var. to store old company id     
lcComp_ID   = " "    && var. to store new company id  
lcCompany   = " "    && var. to return selected (company id,name) from popup
lcComPath   = " "    && var. to store the path for existing source company
lc_SelAcct  = " "    && Temp file name for selected accounts
lc_Temp1    = " "    && Temp file name for group lines
lc_Temp2    = " "    && Temp file name for selected criteria
lcAlTypes   = " "    && Alias name to GLTYPES  in source company
lcAlAcHar   = " "    && Alias name to GLACCHAR in source company
lnNoOfAcc   = 0      && var. to store number of selected accounts
lc_Types    = " "
puPopcom    = 1
lcBut1Stat  = "DISABLE"
lcBut2Stat  = "DISABLE"
lcEmptyTxt  = 'No company Selected'
lcPrompt    = "\<Select"

lsTypes     = 1

llNoContrl  = .T. && Flag to say that this screen has no control pannel.
lcEmptyTxt  = 'No company Selected'

*** Program Initialization
IF !gfSetup()    
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
  lc_SelAcct = gfTempName()
  lc_Temp1   = gfTempName()
  lc_Temp2   = gfTempName()
  lcAlTypes  = gfTempName()
  lcAlAcHar  = gfTempName()
  *** create empty temp file (lc_SelAcct) 
  *** with the same structure of GLTYPES + Inc, CopyStat,cStatus.

  SELECT GLTYPES
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+3,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CopyStat'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 14
  laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+3,1] = 'cStatus'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0

  CREATE TABLE &gcWorkDir.&lc_SelAcct FROM ARRAY laFileStru    

  DO CASE
    CASE _DOS
      lc_Types = "Inc+'   '+cTypeCode+'    '+SUBSTR(cTypeDesc,1,36)+' '+CopyStat"
    CASE _WINDOWS
      lc_Types = "Inc+' '+cTypeCode+'    '+SUBSTR(cTypeDesc,1,36)+'    '+CopyStat"
  ENDCASE
  
ENDIF

SELECT (lc_SelAcct)

*E300683,5 Call *.SPR from screens directory
* DO GlCpTyp.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpTyp.SPR')
*E300683,5 end   
*** case quit this program ...
IF glQuitting 
  *** close temp file (lc_SelAcct) then erase this file. 
  IF USED(lc_SelAcct)
    USE IN ALIAS(lc_SelAcct)
  ENDIF
  ERASE &gcWorkdir.&lc_SelAcct..DBF
  ERASE &gcWorkdir.&lc_SelAcct..CDX

  *** close temp file (lc_Temp1) then erase this file.
  IF USED(lc_Temp1)
    USE IN ALIAS(lc_Temp1)
  ENDIF
  ERASE &gcWorkdir.&lc_Temp1..DBF
  ERASE &gcWorkdir.&lc_Temp1..CDX

  *** close temp file (lc_Temp2) then erase this file.
  IF USED(lc_Temp2 )
    USE IN ALIAS(lc_Temp2 )
  ENDIF
  ERASE &gcWorkdir.&lc_Temp2 ..DBF
  ERASE &gcWorkdir.&lc_Temp2 ..CDX
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
  =lfRefresh()
  *** check if the temp file (lcAlTypes) is used before

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
    *** select (lc_SelAcct) and removes all records.
    *** and refresh the list window
    SELECT (lc_SelAcct)
    lnNoOfAcc = 0 
    lsTypes   = 1     
    SHOW GET lsTypes  
    ZAP       
    SHOW GET lsTypes  
  ENDIF

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

  lnTotalRec = RECCOUNT()

  GO TOP
  IF EOF()
    USE
    SELECT (lc_SelAcct)
    =gfModalGen("TRM02119B00000","Dialog","types")          
    lcBut1Stat = "DISABLE"
    lcBut2Stat = "DISABLE"
    SHOW GET pbSelOne DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbCopy   DISABLE
    SHOW GET lsTypes  ENABLE
    RETURN
  ENDIF

  lnSelected  = 0
 
  SELECT *,'   ' AS 'Inc' ,SPACE(14) AS 'CopyStat','S' AS 'cStatus' ;
         FROM &lcAlTypes;
         INTO DBF &gcWorkDir.&lc_SelAcct
   
    INDEX ON cTypeCode TAG cTypeCode         
    SET ORDER TO TAG cTypeCode         
     
   lsTypes   = 1     
   SHOW GET lsTypes  
  
  SELECT (lc_SelAcct)
  *** set relation between cTypeCode in the temp file and 
  *** GlTypes (account types) in the source company path.
  SET RELATION TO &lc_SelAcct..cTypecode INTO &lcAlTypes 

  SELECT (lc_SelAcct)
  lcBut1Stat = "ENABLE"
  lcBut2Stat = "ENABLE"
  SHOW GET lsTypes  ENABLE
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
SELECT (lc_SelAcct)
IF RECCOUNT() > 0  
  IF EMPTY(&lc_SelAcct..CopyStat) 
    *** check the status for (Inc) field 
    IF !EMPTY(&lc_SelAcct..Inc)
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
SELECT (lc_SelAcct)


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
SELECT (lc_SelAcct)
lnRecNo=RECNO()
IF EMPTY(&lc_SelAcct..CopyStat)
  REPLACE  &lc_SelAcct..Inc WITH IIF(Inc=' û','  ',' û')
ENDIF

IF RIGHT(&lc_SelAcct..cTypeCode,2)="00"
  lcMtype=LEFT(&lc_SelAcct..cTypeCode,1)
  IF &lc_SelAcct..INC=" û"
    REPLACE &lc_SelAcct..INC WITH " û" FOR LEFT(cTypeCode,1)=lcMtype  
*    SHOW GET pbSele,1 PROMPT '\<UnSelect'
  ELSE
    REPLACE &lc_SelAcct..INC WITH "  " FOR LEFT(cTypeCode,1)=lcMtype  
*    SHOW GET pbSele,1 PROMPT '\<Select'
  ENDIF
ENDIF

*** refresh the list and change prompt according to new status.
=lfwList()

IF lnRecNo <= RECCOUNT()
  GOTO lnRecNo
ENDIF
  
lsTypes=lcGrpNo
SHOW GET lsTypes


*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*
FUNCTION  lfvSelAll 
*** check the field status (Inc) for all the file (lc_SelAcct)
*** and change status from unselect to select
lcGrpNo=lsTypes

REPLACE &lc_SelAcct..Inc WITH ' û' FOR EMPTY(&lc_SelAcct..CopyStat)
*** refresh the list and change prompt according to new status.
=lfwList()
lsTypes=lcGrpNo
SHOW GET lsTypes
*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 
*** check the field status (Inc) for all the file (lc_SelAcct)
*** and change status from select   to unselect
lcGrpNo=lsTypes
REPLACE ALL  &lc_SelAcct..Inc WITH '  '
*** refresh the list and change prompt according to new status.
=lfwList()
lsTypes=lcGrpNo
SHOW GET lsTypes

*!**************************************************************************
*!
*!      Function:  lfvInvert 
*!
*!**************************************************************************
*
FUNCTION  lfvInvert 

*** check the status field (Inc) for all the file (lc_SelAcct)
*** and change status from select   to unselect
***                or from unselect to select
lcGrpNo=lsTypes
REPLACE ALL &lc_SelAcct..Inc WITH IIF(Inc=' û','  ',' û') FOR EMPTY(&lc_SelAcct..CopyStat)
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
FUNCTION  lfvCopy

*** start copy selected types
SELECT (lc_SelAcct)
*** check if there is any selected types to copyied
LOCATE  FOR !EMPTY(Inc)
*** case there is no selected types to copyied
IF !FOUND()
   *** Message "No included accounts to copy."
   *** and change the cureent object to (Select) 
   *** then exit from this function.
   =gfModalGen("TRM02103B00000","DIALOG","copy")   
  _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF
*** count the number of included types to the variable lnTotRecs
COUNT FOR !EMPTY(Inc) TO lnTotRecs

*** initialize no. of copied types , current type variable.
lnCopyTypes = 0               && init. the no of copied types.
lnCurRec    = 0               && init. the current type variable.

SELECT (lc_SelAcct)

SCAN FOR !EMPTY(Inc)
  *** tracking current record
  lnCurRec = lnCurRec + 1

  =gfThermo(lnTotRecs,lnCurRec,'Copying Type ...',cTypeCode)
  *** cheking if the copied type Already exists
  IF SEEK (&lc_SelAcct..cTypeCode,"GLTYPES")
    REPLACE &lc_SelAcct..CopyStat WITH 'Already exists'
  ELSE  
    *** function validate that the copied type is not deleted from the 
    *** source file and type ranges does not overlap with type ranges 
    *** in the target company if it is not main type (without ranges) 
    IF lfCheckTyp()
      *** Update gltypes
      SELECT (lc_SelAcct)    
      *** collecting types from temporary file to memory variables
      SCATTER MEMVAR
      SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
      *** inserting selected types into "gltypes" file
      INSERT INTO &gcDataDir.GLTYPES FROM MEMVAR
      *** adding audit information to "gltypes" files
      =gfAdd_Info('GLTYPES')
      *** adjusting copied types counter 
      lnCopyTypes = lnCopyTypes + 1
      *** replacing copystat with proper variable 
      REPLACE &lc_SelAcct..CopyStat WITH 'Copied'
    ENDIF
  ENDIF  
  SELECT (lc_SelAcct)
ENDSCAN
FOR lnCurRec=lnCurRec TO lnTotRecs
  =gfThermo(lnTotRecs,lnCurRec,'Copying type ...',cTypeCode)
ENDFOR
*** refreshing the list
SELECT (lc_SelAcct)
REPLACE INC WITH '  ' FOR !EMPTY(&lc_SelAcct..CopyStat)

SHOW GET lsTypes  
=lfwList()

*** message with no of types copied.
*WAIT ALLTRIM(STR(lnCopyTypes))+" Types copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02240","NOWAIT",ALLTRIM(STR(lnCopyTypes)))

IF lnCopyTypes > 0
*  REPLACE GLACCODS.lAcsUsed WITH .T.
*E300692,1 Chang file name from SYCACCOD TO ACCOD
  *IF SEEK(lcComp_ID,"SYCACCOD")
  *REPLACE SYCACCOD.lAcsUsed WITH .T.
  
  *E300789,7 [BEGIN]
  *IF SEEK(lcComp_ID,"ACCOD")
  IF SEEK('',"ACCOD") 
   REPLACE ACCOD.lAcsUsed WITH .T.
  *E300789,7 [END..]

*E300692,1 end

  ENDIF
ENDIF



*!**************************************************************************
*!
*!      Function:  lfCheckTyp
*!
*!**************************************************************************
*
FUNCTION  lfCheckTyp

*** checking if selected types are deleted from source company
IF EOF(lcAlTypes)       
  REPLACE &lc_SelAcct..CopyStat WITH 'Source deleted'
  RETURN .F.
ENDIF  

*** Checking that the selected type range is not overlaping with
*** any other ranges in the target company file
SELECT GLTYPES
LOCATE FOR GLTYPES.cTypeCode <> &lc_SelAcct..cTypeCode .AND.;
(BETWEEN(VAL(&lc_SelAcct..cTypUacno),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno));      
.OR.;
BETWEEN(VAL(&lc_SelAcct..cTypLacno),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno)))
*** checking that the selected type is not a primary one
*** otherwise bypassing range checking.
IF FOUND() .AND. (!right(&lc_SelAcct..ctypecode,2)="00")
  REPLACE &lc_SelAcct..CopyStat WITH 'Ranges Overlaped'
  RETURN .F.
ELSE
  RETURN .T.
ENDIF  
SELECT (lc_SelAcct)

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function
WAIT " Leave it " WINDOW NOWAIT


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

