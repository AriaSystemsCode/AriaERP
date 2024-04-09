*:************************************************************************
*:
*: Procedure file: GLCPACT.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified: 1/11/94
*:
*:  Procs & Fncts:
*:
*:    FUNCTION  lfwPopup    ***  When popup function                ***
*:    FUNCTION  lfvPopup    ***  Valid popup function               ***
*:    FUNCTION  lfwList     ***  When list function                 ***
*:    FUNCTION  lfvSelOne   ***  Function to select one acount      ***
*:    FUNCTION  lfvSelAll   ***  Function to select all accounts    ***
*:    FUNCTION  lfvSelNon   ***  Function to unselect all accounts  ***
*:    FUNCTION  lfvInvert   ***  Function to invert select status   ***
*:    FUNCTION  lfvCopy     ***  Function to copy selected accounts ***
*:    FUNCTION  lfCheckAct  ***  To check account,segment value     ***
*:    FUNCTION  lfvPrint    ***  Function to print copy stat.report ***
*:    FUNCTION  lfvSelAcc   ***  Function to select accounts to copy***
*:    FUNCTION  lfvClose    ***  Close                              ***
*:    FUNCTION  lfSamAcode  ***  To check if same account structure ***
*:               
*:      Documented 1/11/1994
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E301077,71   Walid abou el-magd (WAM) 03/02/99 
*          FSPRD file was opend by SELECT-SQL command 
*          and there is no command to close it later
*          Instead of that close it by gfCloseFile() .                          
*E300789,7 Walid (WAM) 03/03/99 Make the required modifications due to removing
*           ccomp_id field .
*         
DECLARE laComLst   [1]  && array of companys list
DECLARE laBalRecs  [1]  && array of balance periods
DECLARE laFileStru [1]  

** laComLst   : Array of companys list
** laBalRecs  : Array of balance periods
** lcOldCom   : Var. to store old company id     
** lcComp_ID  : Var. to store new company id  
** lcCompany  : Var. to return selected (company id,name) from popup
** lcComPath  : Var. to store the path for existing source company

** lc_SelAcct : Temp file name for selected accounts
** lc_TSegVal : Temp file name for selected accounts
** lc_Temp1   : Temp file name for group lines
** lc_Temp2   : Temp file name for selected criteria

** lcAlTypes  : Alias name to GLTYPES  in source company
** lcAlAcHar  : Alias name to GLACCHAR in source company
** lcAlAcSeg  : Alias name to GLSEGVAL in source company
** lcAccts    :

** lnNoOfAcc  : Var. to store number of selected accounts
** lnlistLine :
** lnAction   :

** lcBut1Stat :
** lcBut2Stat :
** laCtrStat  : To disable the browse pad in the menu

** lcPrompt   :
** lcEmptyTxt :

** llExitCopy :
** llNoContrl :


STORE " "       TO lcOldCom   , lcComp_ID   , lcCompany   , lcComPath   ,;
                   lc_SelAcct , lc_TSegVal  , lc_Temp1    , lc_Temp2    ,;
                   lcAlTypes  , lcAlAcHar   , lcAlAcSeg   , lcAccts

STORE 0         TO lnNoOfAcc  , lnlistLine  , lnAction

STORE "DISABLE" TO lcBut1Stat , lcBut2Stat  , laCtrStat

lcPrompt    = "\<Select"
lcEmptyTxt  = 'No company Selected'


llExitCopy  = .F.
llNoContrl  = .T.

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
  lc_SelAcct = gfTempName()  && Temp file name for selected accounts
  lc_TSegVal = gfTempName()  && Temp file name for segment values
  lc_Temp1   = gfTempName()  && Temp file name for group lines
  lc_Temp2   = gfTempName()  && Temp file name for selected criteria 

  lcAlTypes  = gfTempName() && Alias name to GLTYPES  in source company
  lcAlAcHar  = gfTempName() && Alias name to GLACCHAR in source company
  lcAlAcSeg  = gfTempName() && Alias name to GLSEGVAL in source company

  *** select window years and the accounts periods for the 
  *** active company from SYCFSPRD into array laBalRecs
*  SELECT SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
         FROM &gcSysHome.SYCFSPRD;
         WHERE sycfsprd.ccomp_id=gcAct_Comp ;
         ORDER BY SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
         INTO ARRAY laBalRecs

*E300692,1 Change file name and path from sycfsprd to fsprd
  *SELECT SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
         FROM &gcSysHome.SYCFSPRD;
         WHERE sycfsprd.ccomp_id=gcPrnt_Cmp ;
         ORDER BY SYCFSPRD.CFISFYEAR, SYCFSPRD.CFSPPRDID;
         INTO ARRAY laBalRecs
*E300789,7 [BEGIN]
*  SELECT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
         FROM &gcDataDir.FSPRD;
         WHERE fsprd.ccomp_id=gcPrnt_Cmp ;
         ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
         INTO ARRAY laBalRecs
  SELECT FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
         FROM &gcDataDir.FSPRD;
         ORDER BY FSPRD.CFISFYEAR, FSPRD.CFSPPRDID;
         INTO ARRAY laBalRecs
*E300789,7 [END..]
*E300692,1 end       
*E301077 [BEGIN]
=gfCloseFile('FSPRD')
*E301077 [END..]
  *** create empty temp file (lc_TSegVal) 
  *** with the same structure of GLSEGVAL
  SELECT GlSegVal

  =AFIELDS(laFileStru)
  CREATE TABLE &gcWorkDir.&lc_TSegVal FROM ARRAY laFileStru 



  *** create empty temp file (lc_SelAcct) 
  *** with the same structure of GLACCHAR + Inc, CopyStat,cStatus.
  SELECT GLACCHAR
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+3,4]

  laFileStru[lnFileStru+1,1] = 'Inc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 3 
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CopyStat'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 15
  laFileStru[lnFileStru+2,4] = 0

  laFileStru[lnFileStru+3,1] = 'CSTATUS'
  laFileStru[lnFileStru+3,2] = 'C'
  laFileStru[lnFileStru+3,3] = 1
  laFileStru[lnFileStru+3,4] = 0

  CREATE TABLE &gcWorkDir.&lc_SelAcct FROM ARRAY laFileStru 

  INDEX ON  cAcctCode TAG cAcctCode
  SET ORDER TO TAG cAcctCode
 
  DO CASE
   CASE _DOS
     lcAccts = "Inc+' '+cTypeCode+'  '+SUBSTR(cAcctCode,1,lnAcsSegSz)+'   '+SUBSTR(cAccnlDes,1,41-lnAcsSegSz)+'  '+CopyStat"
   CASE _WINDOWS
     lcAccts = "Inc+' '+cTypeCode+'  '+SUBSTR(cAcctCode,1,lnAcsSegSz)+' '+SUBSTR(cAccnlDes,1,30-lnAcsSegSz)+SPACE(11)+CopyStat"
  ENDCASE
  
ENDIF

SELECT (lc_SelAcct)  
*E300683,5 Call *.SPR from screens directory
* DO GlCpAct.SPR 
DO (gcScrDir + gcWinAppl + '\GlCpAct.SPR')
*E300683,5 end   
*** case quit this program ...
IF glQuitting 
  *** close temp file (lcAlAchar) then erase this file. 
  IF USED(lcAlAchar)
    SELECT(lcAlAchar)
    USE
  ENDIF
  *** close temp file (lcAlTypes) then erase this file.  
  IF USED(lcAlTypes)
    SELECT(lcAlTypes)
    USE
  ENDIF
  *** close temp file (lcAlAcSeg) then erase this file.   
  IF USED(lcAlAcSeg)
    SELECT(lcAlAcSeg)
    USE
  ENDIF
  *** close temp file (lc_TSegVal) then erase this file. 
  IF USED(lc_TSegVal)
    SELECT(lc_TSegVal)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_TSegVal..DBF
  ERASE &gcWorkdir.&lc_TSegVal..CDX
  *** close temp file (lc_SelAcct) then erase this file. 
  IF USED(lc_SelAcct)
    SELECT(lc_SelAcct)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_SelAcct..DBF
  ERASE &gcWorkdir.&lc_SelAcct..CDX
  *** close temp file (lc_Temp1) then erase this file.
  IF USED(lc_Temp1)
    SELECT(lc_Temp1)
    USE
  ENDIF
  ERASE &gcWorkdir.&lc_Temp1..DBF
  ERASE &gcWorkdir.&lc_Temp1..CDX
  *** close temp file (lc_Temp2) then erase this file.
  IF USED(lc_Temp2 )
    SELECT(lc_Temp2 )
    USE
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
*** company from the list popup menu.
*** and return the path for selected company
*** returns 2 characters from lcCompany that contains 
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
  =lfRefresh()    && refresh says filed on the screen


  *** check if the temp file (lcAlAcHar) is used before
  IF USED(lcAlAcHar)
    *** if used then select this area 
    SELECT (lcAlAcHar)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use GLACCCHAR from the source company in the path lcComPath
  *** and set index to order tag ACCTCODE
  USE &lcComPath.GLACCHAR AGAIN ALIAS  &lcAlAcHar ORDER TAG ACCTCODE
  
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

  
  *** check if the temp file (lcAlAcSeg) is used before
  IF USED(lcAlAcSeg)
    *** if used then select this area 
    SELECT (lcAlAcSeg)
  ELSE   
    *** else select a new empty work area
    SELECT 0   
  ENDIF  
  *** use GLSEGVAL from the source company in the path lcComPath
  *** and set index to order tag ACSSEGVAL
  USE &lcComPath.GLSegVal AGAIN ALIAS  &lcAlAcSeg  ORDER TAG ACSSEGVAL      


  *** set relation between cAcctCode in the temp file and 
  *** GlAcChar (account chart) in the source company path.
  SELECT (lc_SelAcct)
  SET RELATION TO &lc_SelAcct..cacctcode INTO &lcAlAcHar 
  
  *** check if the the first row in the array contains empty
  *** in case if this is the first time that change active company.
  IF EMPTY(laComLst[1,2])
    *** delete the first raw in the array and redimension the array.
    =ADEL(laComLst,1)
    DIMENSION laComLst[Alen(lacomlst,1)-1,2]
      IF _WINDOWS
        puPopCom = puPopCom - 1
        SHOW GET puPopCom ENABLE
      ENDIF
  ELSE
    *** select (lc_SelAcct) and removes all records.
    *** and refresh the list window
    SELECT (lc_SelAcct)
    lnNoOfAcc = 0 
    lsAccts   = 1     
    SHOW GET lsAccts  
    ZAP       
    SHOW GET lsAccts   
  ENDIF

  *** disable all objects exepet the (list) and  (select accounts to
  ***  copy) options.
  lcBut1Stat =       "ENABLE"
  lcBut2Stat =       "DISABLE"
  SHOW GET pbSelAcc   ENABLE
  SHOW GET pbSelOne   DISABLE
  SHOW GET pbSelAll   DISABLE
  SHOW GET pbSelNon   DISABLE
  SHOW GET pbInvert   DISABLE
  SHOW GET pbCopy     DISABLE
  SHOW GET rbCopyType DISABLE
  SHOW GET lsAccts  
  
ENDIF


*!**************************************************************************
*!
*!      Function:  lfwList
*!
*!**************************************************************************
*
FUNCTION  lfwList
*** check if the number of accounts in the list greater than 1 
IF lnNoOfAcc > 0 
  IF !EMPTY(CopyStat)
      lcPrompt = "Select"
      
      SHOW GET pbSelOne,1 PROMPT 'Select' DISABLE
  ELSE 
    *** check the status for (Inc) field 
    IF !EMPTY(Inc)
      *** case the selected .. change the prompt to 'unselect'
      lcPrompt = "\<Unselect"
      SHOW GET pbSelOne,1 PROMPT '\<Unselect' ENABLE
    ELSE                                   
      *** case the unselected .. change the prompt to 'select'
      lcPrompt = "\<Select"
      SHOW GET pbSelOne,1 PROMPT '\<Select' ENABLE
    ENDIF
  ENDIF
ENDIF  


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
IF EMPTY(CopyStat)
  REPLACE Inc WITH IIF(Inc=' û','  ',' û')
  *** refresh the list and change prompt according to new status.
  SHOW GET lsAccts
  =lfwList()
ENDIF
*!**************************************************************************
*!
*!      Function:  lfvSelAll 
*!
*!**************************************************************************
*

FUNCTION  lfvSelAll 
*** check the field status (Inc) for all the file (lc_SelAcct)
*** and change status from unselect to select

lnlistLine = lsAccts 
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),' û','')
*** refresh the list and change prompt according to new status.
lsAccts = lnlistLine 
SHOW GET lsAccts
=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvSelNon 
*!
*!**************************************************************************
*
FUNCTION  lfvSelNon 
*** check the field status (Inc) for all the file (lc_SelAcct)
*** and change status from select   to unselect
lnlistLine = lsAccts 
REPLACE ALL Inc WITH IIF(EMPTY(CopyStat),'  ','')

*** refresh the list and change prompt according to new status.
lsAccts = lnlistLine 
SHOW GET lsAccts
=lfwList()

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
lnlistLine = lsAccts 
REPLACE  ALL Inc WITH IIF(EMPTY(CopyStat),IIF(Inc=' û','  ',' û'),'')
*** refresh the list and change prompt according to new status.
lsAccts = lnlistLine 
SHOW GET lsAccts
=lfwList()

*!**************************************************************************
*!
*!      Function:  lfvCopy 
*!
*!**************************************************************************
*
FUNCTION  lfvCopy
*** start copy selected accounts
SELECT (lc_SelAcct)
lnlistLine = lsAccts 
llExitCopy = .F.
*** count the number of included accounts to the variable lnTotRecs
*** check if there is any selected accounts to copyied
COUNT FOR !EMPTY(Inc) TO lnTotRecs

*** case there is no selected accounts to copyied
IF lnTotRecs = 0 
   *** Message "No included accounts to copy."
   *** and change the current object to (Select) 
   *** then exit from this function.
   =gfModalGen("TRM02103B00000","DIALOG","copy")   
   _CUROBJ=OBJNUM(pbSelOne)
  RETURN
ENDIF

lnAction = gfModalGen("TRM02192B00006","Dialog")

lnCopyActs = 0               && init. the no of copied accounts.
lnCurRec   = 0               && init. the current account variable.

lc_Balance = gfTempName()    && Temp file name for balances.

  *** create empty temp file (lc_Balance) 
  *** with the same structure of GLACBALS.
  SELECT GLACBALS
  COPY STRUCTURE TO &gcWorkDir.&lc_Balance
 
  lcTime = gfGetTime() 
  ldDate = DATE()

 SELECT GLACBALS
 *** for every account we need to create a number of records 
 *** matchs the number of periods for fiscal years with 0 values
  FOR lnCount = 1 TO ALEN(laBalRecs,1)
    INSERT INTO &gcWorkDir.&lc_Balance ;
       (cAcctCode,cFisfYear,cFspPrdid,nacbptddr,nacbptdcr,;
        nacbytddr,nacbytdcr,nacbopbal,nacbclbal,cAdd_User,;
        dAdd_Date,cAdd_Time);
        VALUES ('00',laBalRecs[lnCount,1],laBalRecs[lnCount,2],;
        0,0,0,0,0,0,gcUser_Id,ldDate,lcTime)
  ENDFOR

SELECT (lc_SelAcct)

*** start copying all selected accounts only
SCAN FOR !EMPTY(Inc) .AND. ! llExitCopy 
lnCurRec = lnCurRec + 1
=gfThermo(lnTotRecs,lnCurRec,'Copying account ...',cAcctCode)

  *** searches the current dbf (GLACCHAR) if the current account
  *** number in the dbf (lc_SelAcct) is Already exists
  IF SEEK (cAcctCode,"GLACCHAR")
    *** if found then change the copy status field to "Already exists"
    REPLACE &lc_SelAcct..CopyStat WITH 'Already exists'
  ELSE  
    *** check the account number type and if the accounte value 
    *** lies between the values of any type in dbf (GLTYPES).
    IF lfCheckAct()
      *** if function lfCheckAct return .T.
      *** update dbf GLACCHAR and add the audit information.
      SELECT (lc_SelAcct)    
      SCATTER MEMVAR
      SCATTER FIELDS Llok_Stat,Clok_user,Dlok_Date,Clok_time MEMVAR BLANK
      INSERT INTO &gcDataDir.GLACCHAR FROM MEMVAR
      =gfAdd_Info('GLACCHAR')
      *** increment the number of copyied accounts by 1 
      lnCopyActs = lnCopyActs + 1

      SELECT (lc_Balance)   
      *** replace the account number in the dbf (lc_balance) with 
      *** the current account number in the dbf (lc_SelAcct)
      REPLACE ALL &lc_Balance..cAcctCode WITH &lc_SelAcct..cAcctCode
      *** then update glbalance.
      SELECT GLACBALS
      APPEND FROM &gcWorkDir.&lc_Balance
      REPLACE &lc_SelAcct..CopyStat WITH 'Copied'
    ENDIF
  ENDIF  
  
  SELECT (lc_SelAcct)
ENDSCAN

  *** close temp file (lc_Balance) then erase this file.
  IF USED(lc_Balance)
    USE IN ALIAS(lc_Balance)
  ENDIF

  ERASE &gcWorkdir.&lc_Balance..DBF
  ERASE &gcWorkdir.&lc_Balance..CDX

IF lnTotRecs <> lnCurRec
  IF llExitCopy  
    =gfThermo(lnTotRecs,lnTotRecs,'Copying account ...',cAcctCode)  
  ELSE
    FOR lnCurRec = lnCurRec TO lnTotRecs
      =gfThermo(lnTotRecs,lnCurRec,'Copying account ...',cAcctCode)
    ENDFOR  
  ENDIF  
ENDIF

SELECT (lc_SelAcct)
REPLACE ALL  Inc WITH '  '
lsAccts = lnlistLine 
SHOW GET lsAccts  
*WAIT ALLTRIM(STR(lnCopyActs))+" Accounts copied from source company..." ;
     WINDOW NOWAIT
=gfWait("02233","NOWAIT",ALLTRIM(STR(lnCopyActs)))
=lfwList()
*!**************************************************************************
*!
*!      Function:  lfCheckAct 
*!
*!**************************************************************************
*
FUNCTION  lfCheckAct

*** check if the type for the current account is exist in dbf (GLTYPES)
IF !SEEK(cTypeCode,"GLTYPES")       
  *** case not found this type. 
  *** replace the copy status field for this account with 'Type not found'
  *** and return false.
  REPLACE &lc_SelAcct..CopyStat WITH 'Type not found'
  RETURN .F.
ELSE
  *** case found the type in the dbf (GLTYPES)
  *** check if this type between the values of
  ***  ( lower limit       , upper limit       ) 
  ***    GLTYPES.cTyplacno , GLTYPES.cTypuacno
  IF !BETWEEN(VAL(cAcctCode),VAL(GLTYPES.cTyplacno),VAL(GLTYPES.cTypuacno))
    *** case the type is out of range ... 
    *** replace the copy status field for this account with 'Out of range'
    *** and return false.
    REPLACE &lc_SelAcct..CopyStat WITH 'Out of range'
    RETURN .F.
  ENDIF 
ENDIF

*** removes all records from a dbf (lc_TSegVal) 
*** and calling global function (gfSubStr) with 3 parameters 
*** frist parameter the current account code.
*** 2 nd  parameter array name.
*** 3 rd  parameter separeator for the account code (-).
*** this function extracts and returns every segment value
*** into array (laAcctSegs)

SELECT (lc_TSegVal)
ZAP
DECLARE laAcctSegs[1]
SELECT (lc_SelAcct)
=gfSubStr(cAcctCode,@laAcctSegs,'-')

*** check if this account code still exist in the master file (lcAlAcHar)
*** for the source company .. 
IF !EOF(lcAlAcHar)       
  *** case account code exist in source company..

  IF lnAction = 1 
    SELECT (lc_SelAcct)

    IF !EMPTY(&lc_SelAcct..cSegCaFlo)
      SELECT GLCFITEM
      SET ORDER TO cficode
      IF ! SEEK(&lc_SelAcct..cSegCaFlo,"GLCFITEM")       
        SELECT (lc_SelAcct)
        lnCopyMode = gfModalGen("TRM02193B02018","Dialog",;
                     ALLTRIM(cAcctCode)+'|C.F. code'+;
                     '|C.F. code: '+ALLTRIM(&lc_SelAcct..cSegCaFlo))
        DO CASE
          CASE lnCopyMode = 1    && Intialize fields to blank
            REPLACE &lc_SelAcct..cSegCaFlo WITH ''

          CASE lnCopyMode = 3    && Ignore copying this account
            REPLACE &lc_SelAcct..CopyStat WITH 'C.F. not found'
            RETURN .F.

          CASE lnCopyMode = 4    && Exit
            llExitCopy = .T.
            RETURN .F.

        ENDCASE
      ENDIF  
    ENDIF

    IF !EMPTY(&lc_SelAcct..cSegAutDs)   
      SELECT GLAUTHD
      SET ORDER TO TypeCode
      IF !SEEK('D'+&lc_SelAcct..cSegAutDs,"GLAUTHD")       
        SELECT (lc_SelAcct)
        *** Account ð has ð, which deos not exist in the target company. 
        *** You can copy the account without the ð (blank), copy it 
        *** <as it is>, ignore copying the account, or stop copying the 
        *** rest of accounts.In case of copying <as it is> you are 
        *** responsable for adding this ð.
        
        lnCopyMode = gfModalGen("TRM02193B02018","Dialog",;
                     ALLTRIM(cAcctCode)+'|A.D. code'+;
                     '|A.D. code: '+ALLTRIM(&lc_SelAcct..cSegAutDs))
        DO CASE
          CASE lnCopyMode = 1    && Intialize fields to blank
            REPLACE &lc_SelAcct..cSegAutDs WITH ''

          CASE lnCopyMode = 3    && Ignore copying this account
            REPLACE &lc_SelAcct..CopyStat WITH 'A.D. not found'
            RETURN .F.

          CASE lnCopyMode = 4    && Exit
            llExitCopy = .T.
            RETURN .F.

        ENDCASE
      ENDIF  
    ENDIF

  ENDIF

  FOR lnCount = 1 TO ALEN(laAcctSegs,1)
    *** for every segment in array (laAcctSegs) check if this
    *** segment value is exist in the target company (GLSEGVAL)
    IF !SEEK(STR(lnCount,1)+laAcctSegs[lnCount],'GLSEGVAL') 
      *** case segment value not exist ....
      *** copy this segment values record from source company (lcAlAcSeg)
      *** into dbf (lc_TSegVal), and add audit information. 
      SELECT (lcAlAcSeg)
      SEEK STR(lnCount,1)+laAcctSegs[lnCount]
      SCATTER MEMVAR 
      INSERT INTO &gcWorkDir.&lc_TSegVal FROM MEMVAR 
      =gfAdd_Info(lc_TSegVal) 
    ENDIF
  ENDFOR     
ELSE
  *** case account code deleted from source company..
  *** replace the copy status field for this account with 'Source deleted'
  *** and return false.
  REPLACE &lc_SelAcct..CopyStat WITH 'Source deleted'
  RETURN .F.
ENDIF  


*** check if there is any record in the temp (lc_TSegVal)
*** if found .. append all this new segment values from the
*** temp file (lc_TSegVal) to the master file (GLSEGVAL) in 
*** the target company.
IF RECCOUNT(lc_TSegVal) > 0 
  SELECT GLSEGVAL
  APPEND FROM &gcWorkDir.&lc_TSegVal
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvPrint
*!
*!**************************************************************************
*
FUNCTION  lfvPrint
*** printing function

*!**************************************************************************
*!					
*!      Function:  lfvSelAcc 
*!
*!**************************************************************************
*

FUNCTION  lfvSelAcc
*** select accounts form the source company to be copied.
*E300683,5 Call programs from PRGS directory
*DO glSlAct.PRG WITH lc_Temp1,lc_Temp2,lc_SelAcct,lnNoOfAcc,lcAlTypes,lcAlAchar,.T.,.T.
DO (gcAppHome + gcWinAppl + '\glSlAct.PRG ') WITH lc_Temp1,lc_Temp2,lc_SelAcct,lnNoOfAcc,lcAlTypes,lcAlAchar,.T.,.T.
*E300683,5 end

SELECT (lc_SelAcct)

*** check if the number of accounts greater than 0 
*** in this case enable all objects, and change current object to list.

IF lnNoOfAcc > 0 
  lcBut2Stat =       "ENABLE"
  SHOW GET pbSelOne   ENABLE
  SHOW GET pbSelAll   ENABLE
  SHOW GET pbSelNon   ENABLE
  SHOW GET pbInvert   ENABLE
  SHOW GET pbCopy     ENABLE
  SHOW GET rbCopyType ENABLE  
  lsAccts   = 1     
  _CUROBJ= OBJNUM(lsAccts)
ELSE 
  *** else disable all objects, and change current object to 'select accounts' 
  lcBut2Stat =       "DISABLE"
  SHOW GET pbSelOne   DISABLE
  SHOW GET pbSelAll   DISABLE
  SHOW GET pbSelNon   DISABLE
  SHOW GET pbInvert   DISABLE
  SHOW GET pbCopy     DISABLE
  SHOW GET rbCopyType DISABLE    
  _CUROBJ= OBJNUM(pbSelAcc)
ENDIF

SHOW GET lsAccts  
=lfwList()


*!**************************************************************************
*!
*!      Function:  lfvClose
*!
*!**************************************************************************
*
FUNCTION  lfvClose
glQuitting  = .T.  

