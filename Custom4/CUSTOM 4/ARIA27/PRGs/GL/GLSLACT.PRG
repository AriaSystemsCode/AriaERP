*:*************************************************************************
*:                                                                      ***
*: Procedure file: GLSLACT.PRG                                          ***
*:                                                                      ***
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Hany Wadie                                           ***
*:      Copyright (c)                                                   ***
*:  Last modified: 01/09/1994                                           ***
*:                                                                      ***
*:  Procs & Fncts:                                                      ***
*:                                                                      ***
*:    FUNCTION: lfwAcType   *** when function for AcType List           ***
*:    FUNCTION: lfvAcType   *** Validatin  for AcType List              ***
*:    FUNCTION: lfvSeleAll  *** Validation for Select All push button   ***
*:    FUNCTION: lfvInvert   *** Validation for Invert push button       ***
*:    FUNCTION: lfvSeleNo   *** Validation for Select None button       ***
*:    FUNCTION: lfvActCode  *** Validation for Acount code variable     ***
*:    FUNCTION: lfvAdd      *** Validation for Add push button          ***
*:    FUNCTION: lfTherm     *** Thermometer for selecting accounts      ***
*:                                                                      ***
*:    Documented 01/12/1994                                             ***
*:*************************************************************************
*B600382,1 Reham On 06/06/95 
*B600382,1 Change the replacing condition because in the add mode it was not
*B600382,1 saving only the replaced accounts, it was merging everything.
*
*B600386,1 Reham On 06/07/95
*B600386,1 Change the picture of the account mask for the possibility to
*B600386,1 make it blank if I need.
*
*B600388,1 Reham On 06/07/95
*B600388,1 We desovered that this screen used to select accounts only
*B600388,1 & it has been called from 3 places (GLBUDGT-GLGROUP-GLCPACT)
*B600388,1 & there is no need to resize the window under dos or windows
*B600388,1 So the following has been done.
*B600388,1 Move the next 2 line of code from SCX setup (DOS & WINDOWS)
*B600388,1  #WFROM  TO IIF(llFlag,20,17),56
*B600388,1  MOVE WINDOW awdGlSlAct CENTER
*B600388,1 Move all the positions directives from the comments of 
*B600388,1 2 buttons <OK> & <CANCEL> & also from the lines around
*B600388,1 them.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*:*************************************************************************
*
***  following are the paramenter detials :
*** lc_TempTyp : temporary file contains account types and description
*** lc_NewAcc  : temporary file for selected criteria 
*** lc_TempDst : temporary file originally contains group lines
*** lnGrplins  : no of goup lines found in the list : "lsActType"
*** lcAlias1   : alias for "gltypes"  file
*** lcAlias2   : alias for "glacchar" file
*** llUpdate   : logical flag to detemine whatever to update or not
*** llFlag     : logical Parameter to either deal with accounts or types

PARAMETERS lc_TempTyp,lc_NewAcc,lc_TempDst,lnGrpLins,lcAlias1,lcAlias2,llUpdate,llFlag

*B600386,1 Reham On 06/07/95
*B600386,1 Change the picture of the account mask for the possibility to
*B600386,1 make it blank if I need. (Add the var. in the SCX also).
lcTmpMask = ALLTRIM(STRTRAN(lcAcsMask,'9','X'))

lnSelected= 0
lnRitsize = 0
lnTemp=0
IF _DOS
  lnBox = IIF(llFlag,20,17)
ELSE
  lnBox = IIF(llFlag,37.4,22.3)
ENDIF  

lcTitle = IIF(llFlag,'Select accounts','Select types')

*** check that the first alias paremeter is passed & not empty
*** otherwise use the default alias "gcDataDir" 
IF !EMPTY(lcAlias1)
  lcTypes=lcAlias1 
ELSE
  lcTypes="glTypes"
ENDIF

*** check that the Second alias paremeter is passed & not empty
*** otherwise use the default alias "gcDataDir" 
IF ! EMPTY(lcAlias2) 
  lcAcchar=lcAlias2
ELSE
  lcAcchar= "glAcchar"
ENDIF

SELECT (lcTypes)
GO TOP
IF EOF(lcTypes)
  =gfModalGen("TRM02111B00000","ALERT")  
  RETURN
ELSE
  IF llFlag
    SELECT (lcAcchar)
    GO TOP
    IF EOF(lcAcchar)
      =gfModalGen("TRM02112B00000","ALERT")
      RETURN  
    ENDIF
  ENDIF
ENDIF

*** declaring ladata array as external array
EXTERNAL ARRAY laData
***  creating temp. file contains accounts type & description
SELECT cTypeCode,cTypeDesc," " AS 'cflag';
       FROM &lcTypes; 
       INTO DBF &gcWorkDir.&lc_TempTyp

*** indexing the file "lc_TempTyp" on cTypeCode
INDEX ON CTYPECODE TAG CTYPECODE        
*** preparing the string "lcType" for the list
DO CASE
  CASE _DOS
    lcType   = "&lc_TempTyp..Cflag+'  '+&lc_TempTyp..Ctypecode+'  '+&lc_TempTyp..Ctypedesc"
  CASE _WINDOWS
    lcType   = "&lc_TempTyp..Cflag+'   '+&lc_TempTyp..Ctypecode+'   '+&lc_TempTyp..Ctypedesc"
ENDCASE  
*** positioning the list pointer to list begining
lsAcType = 1
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)
*** filling the account code variable with "*"
lcActCode  = REPLICATE ('*',lnAcsSegSz)
*** initializing "lnTypeRec" with 0
lnTypeRec  = 0

*** calling screen which adds accounts accourding to their type & scope
*E300683,5 Call *.SPR from screens directory
* DO glSlAct.SPR 
DO (gcScrDir + gcWinAppl + '\glSlAct.SPR')
*E300683,5 end   

*!**************************************************************************
*!
*!      FUNCTION: lfwAcType
*!
*!**************************************************************************
* When function for AcType List
FUNCTION lfwAcType
IF &lc_TempTyp..cFlag="û"
 SHOW GET pbSele,1 PROMPT '\<UnSelect'
ELSE
 SHOW GET pbSele,1 PROMPT '\<Select'
ENDIF       
*** showing the list "lsAcTyp"
SHOW GET lsAcType 


*!**************************************************************************
*!
*!      FUNCTION: lfvAcType
*!
*!**************************************************************************
* Validatin for AcType List
FUNCTION lfvAcType
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)
*** inverting cFlag status; except for types correspondes to selected
*** accounts scope
REPLACE &lc_TempTyp..cFlag WITH IIF(&lc_TempTyp..cFlag="û" .AND. ;
        RECNO(lc_TempTyp) <> lnTypeRec ," ","û")
*** changing the prompt of push button pbsele according to cFlag status
IF &lc_TempTyp..cFlag="û"
 SHOW GET pbSele,1 PROMPT '\<UnSelect'
ELSE
 SHOW GET pbSele,1 PROMPT '\<Select'
ENDIF       
*** If selected type is a main type; subtypes also will selected
*** or not according to the main type; also the prompt of "pbsele"
*** will be adjuted according to cFlag sataus of the main type
IF RIGHT(&lc_TempTyp..cTypeCode,2)="00"
  lcMtype=LEFT(&lc_TempTyp..cTypeCode,1)
  IF &lc_TempTyp..cFlag="û"
    REPLACE &lc_TempTyp..cFlag WITH "û" FOR LEFT(cTypeCode,1)=lcMtype  
    SHOW GET pbSele,1 PROMPT '\<UnSelect'
  ELSE
    REPLACE &lc_TempTyp..cFlag WITH " " FOR LEFT(cTypeCode,1)=lcMtype  
    SHOW GET pbSele,1 PROMPT '\<Select'
  ENDIF
ENDIF
*** showing the list "lsAcTyp"
SHOW GET lsAcType 

*!**************************************************************************
*!
*!      FUNCTION: lfvSeleAll
*!
*!**************************************************************************
* Validation for Select All push button
FUNCTION lfvSeleAll
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)

REPLACE ALL &lc_TempTyp..cFlag WITH "û"
SHOW GET pbSele,1 PROMPT '\<UnSelect'
SHOW GET lsAcType  

*!**************************************************************************
*!
*!      FUNCTION: lfvInvert
*!
*!**************************************************************************
* Validation for Invert push button
FUNCTION lfvInvert
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)
GO TOP
lcActype=RECNO()
REPLACE ALL &lc_TempTyp..cFlag WITH IIF(cFlag="û" .AND.;
            RECNO(lc_TempTyp) <> lnTypeRec ," ","û")
GOTO lcActype
IF &lc_TempTyp..cFlag="û"
 SHOW GET pbSele,1 PROMPT '\<UnSelect'
ELSE
 SHOW GET pbSele,1 PROMPT '\<Select'
ENDIF       
SHOW GET lsAcType  

*!**************************************************************************
*!
*!      FUNCTION: lfvSeleNo
*!
*!**************************************************************************
* Validation for Select None button
FUNCTION lfvSeleNo
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)
*** Blanking cFlag in all the temp. file 
REPLACE ALL &lc_TempTyp..cFlag WITH " " FOR RECNO(lc_TempTyp) <> lnTypeRec
*** Showing the push button "pbSele" with prompt "Select"
SHOW GET pbSele,1 PROMPT '\<Select'
*** Showin the list lsAcTyp
SHOW GET lsAcType  

*!**************************************************************************
*!
*!      FUNCTION: lfvActCode
*!
*!**************************************************************************
* Validation for Acount code variable
FUNCTION lfvActCode

IF VAL(lcActCode) > 0 
  *** selecting data file glTypes
  SELECT (lcTypes)
  *** searhing for the corresponding type of the selected acccounts   
  LOCATE FOR BETWEEN(VAL(lcActCode),VAL(cTyplacno),VAL(cTypuacno))
  IF FOUND()
    *** finding the required type to checked
    IF SEEK(cTypeCode,lc_TempTyp)  
      *** checking the rquired type with "û"
      REPLACE ALL &lc_TempTyp..cFlag WITH "û"
      *** variable will hold the selected type record number
      *** in the temp. file "lc_TempTyp"
      lnTypeRec = RECNO(lc_TempTyp) 
      *** showing the list "lsAcType"
      SHOW GET lsAcType  
    ENDIF   
  ENDIF 
  ELSE
  *** No type selected; so record number will be zero 
  lnTypeRec=0
ENDIF
*** adjusting account code variable length
lcActCode   = SUBSTR(lcActCode,1,lnAcsSegSz)
*** substituting any spaces in the account code with "*"
lcActCode   = STRTRAN(lcActCode," ","*")
*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)

*!**************************************************************************
*!
*!      FUNCTION: lfvAdd
*!
*!**************************************************************************
* Validation for Add push button
FUNCTION lfvAdd

*** selecting temp. file "lc_TempTyp"
SELECT (lc_TempTyp)
*** checking if at least one type selelcted
LOCATE FOR cFlag = "û"
IF !FOUND()
  IF llFlag    
    *** message "You Have to select account type"
    =gfModalGen("TRM02094B00000","DIALOG","account type")
    *** terminate the Function "lfvAdd"
    RETURN
  ELSE
    *** message "You Have to select type"
    =gfModalGen("TRM02094B00000","DIALOG","type")
    *** terminate the Function "lfvAdd"
    RETURN
  ENDIF
ENDIF

*** Flag used to determine whatever processing is done with types
*** and ranges (if false) or with accounts and types (if true)
IF llFlag
  *** substituting any "*" in the account code with "_"
  lcActMask   = STRTRAN(lcActCode,"*","_")
  *** counting all records in the account chart file
  lnTotalRec  = RECCOUNT(lcAcchar) 
  *** initializing "lnSelected" (lines selected) to zero
  lnSelected  = 0
  
  *** creating temp. file "lc_NewAcc" which contains selected accounts
  *** satisfying the selected accounts type & scope
  *** while calling the themometer Function 
  SELECT laData[1] AS 'cGrpCode',&lcAcchar..*,;
         'I' AS 'cGrdStat', 0 AS 'nRecNO',"A" AS 'cStatus';
    FROM &lcAcchar,&gcWorkDir.&lc_TempTyp;
   WHERE &lcAcchar..cAcctCode LIKE lcActMask .AND.;
         (&lc_TempTyp..cFlag="û" .AND. ;
         &lcAcchar..cTypeCode = &lc_TempTyp..cTypeCode) .AND.;
         lfTherm();
    INTO DBF &gcWorkDir.&lc_NewAcc
  
  *** terminating global Function thermometer
  IF lnTotalRec <> lnSelected 
    FOR lnSelected=lnSelected TO lnTotalRec
      =gfThermo(lnTotalRec,lnSelected,'Selecting accounts..',' ')
    ENDFOR
  ENDIF
  
  *** If no accounts selected; issue a message "No Accounts Selected"
  *** then terminating the the Function "lfvAdd"
  IF _TALLY = 0
     =gfModalGen("TRM02095B00000","DIALOG","accounts")
    *** selecting temp. file "lc_TempTyp"
    SELECT (lc_TempTyp)
    RETURN
  ENDIF  
  
  *** The module is used only to select accounts
  IF llUpdate=.F.
    CLEAR READ
    RELEASE WINDOW awdGlSlAct
    RETURN
  ENDIF
  *** message "Adding accounts" with options (Add,Cancel)
  *** if the user choice is cancel; teminating the the Function
  lcParam=ALLTRIM(STR( _TALLY))+IIF(_TALLY=1,"| is selected","|s are selected")
  IF gfModalGen("TRM02072B02008","DIALOG",lcParam) = 2
    RETURN
  ENDIF
  *** If there is lines from before
  IF lnGrpLins > 0
    *** message "Adding New Records" with options" :
    *** replace,merge,cancel  
    lnOption = gfModalGen("TRM02073B02012","DIALOG")
    DO CASE
      *** Replace option is selected
      CASE lnOption = 1                  &&   ( REPLACE )
        *** selecting temp. file "lc_TempDst"
        SELECT (lc_TempDst)
        *** modifing the flag field "cFlag" with "M" for modify
        *** status except for the flag "A" for Add status
        
        *B600382,1 Reham On 06/06/95 
        *B600382,1 Change the replacing condition because in the add mode
        *B600382,1 it was not saving only the replaced accounts, it 
        *B600382,1 was merging everything.
        *REPLACE ALL cStatus WITH "D" FOR cStatus <> "A"
        REPLACE ALL cStatus WITH SUBSTR("DDS",AT(cStatus,"MSA"),1)
        
        *** deleting old accounts
        DELETE ALL
        *** keeping track of deleted record count in the file "lc_TempDst"
        lnOldRecs  = RECCOUNT(lc_TempDst)
        *** counting the records in the file "lc_NewAcc"
        lnTotalRec = RECCOUNT(lc_NewAcc )
        ***  appending from "lc_NewAcc" while calling the thermometer Function
        APPEND FROM &gcWorkDir.&lc_NewAcc ;
               FOR gfThermo(lnTotalRec,RECNO(lc_TempDst)-lnOldRecs,;
                   'Adding account..',cAcctCode)
        IF lnTotalRec <> RECNO(lc_TempDst)-lnOldRecs
          lnTemp=RECNO(lc_TempDst)-lnOldRecs
          FOR lnTemp=lnTemp TO lnTotalRec
            =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
          ENDFOR
        ENDIF
        *** updating "lnGrpLins" (group lines) variable  
        lnGrpLins = RECCOUNT(lc_NewAcc)
      *** Merge option was selected
      CASE lnOption = 2                  &&   ( MERGE )
        *** selecting temp. file "lc_TempDst"
        SELECT (lc_TempDst)
        *** Cheking that the index is already created
        IF SYS(22)<>"AcctCode"
           *** indexing the file for further seek
           INDEX ON CACCTCODE TAG ACCTCODE ADDITIVE
        ENDIF
        SET ORDER TO TAG AcctCode
        *** reindexing is issued because the module will be used separtely
        REINDEX 
        *** saving group lines count before merge
        lnOldGrLin = lnGrpLins
        *** selecting temp. file "lc_NewAcc"
        SELECT (lc_NewAcc)
        *** counting total records to be merged
        lnTotalRec = RECCOUNT()
        *** scaning the file "lc_NewAcc"
        SCAN 
          *** checking that the account code is not present in the
          *** group lines temp. file "lc_TempDst"
          IF !SEEK(&lc_NewAcc..cAcctCode,lc_TempDst)
            *** themometer Function
            =gfThermo(lnTotalRec,RECNO(lc_NewAcc),'Adding account..',cAcctCode)
            lnTemp=RECNO(lc_NewAcc)
            *** copying record data to memory variable
            SCATTER MEMVAR MEMO
            *** selecting temp. file for group lines "lc_TempDst"
            SELECT (lc_TempDst)
            *** adding blank record
            APPEND BLANK
            *** copying record data from memory variable
            *** to the temp. file "lc_TempDst"
            GATHER MEMVAR MEMO
            *** incrementing group lines variable "lnGrpLins"
            lnGrpLins = lnGrpLins + 1
            *** selecting temp. file "lc_NewAcc"
            SELECT (lc_NewAcc) 
          ENDIF
        ENDSCAN
        
        IF lnTotalRec <> RECNO(lc_NewAcc)
          FOR lnTemp=lnTemp TO lnTotalRec
            =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
          ENDFOR
        ENDIF
      CASE lnOption = 3                  &&   ( CANCEL )
        RETURN         
    ENDCASE
  *** There is no lines from before
  ELSE
    *** selecting temp. file "lc_TempDst"
    *** in this case the group previously has no accounts
    *** so total selected lines will be added
    SELECT (lc_TempDst)
    *** counting total records in the temp. file "lc_NewAcc"
    lnTotalRec = RECCOUNT(lc_NewAcc)
    *** appending from temp. file "lc_NewAcc" to temp. file "lc_TempDst"
    APPEND FROM &gcWorkDir.&lc_NewAcc ;
           FOR gfThermo(lnTotalRec,RECNO(lc_TempDst),'Adding account..',;
           cAcctCode)
    IF lnTotalRec <> RECNO(lc_TempDst)
     lnTemp=RECNO(lc_TempDst)
     FOR lnTemp=lnTemp TO lnTotalRec
        =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
     ENDFOR
    ENDIF
     *** updating group lines variable "lnGrpLins"
    lnGrpLins = RECCOUNT(lc_NewAcc)  
  ENDIF  
  *** terminating adding lines to the group session; by terminating the read
  *** and closing opend files
  CLEAR READ
ELSE
  *** counting all records in the types file
  COUNT FOR &lc_TempTyp..cFlag="û" TO  lnTotalRec
  lnTotalRec=lnTotalRec+RECCOUNT(lcTypes)
  *** initializing "lnSelected" (lines selected) to zero
  lnSelected  = 0
  *** creating temp. file "lc_NewAcc" which contains selected accounts
  *** satisfying the selected accounts type & scope
  *** while calling the themometer Function 
  
  SELECT &lcTypes..* FROM &lcTypes,&gcWorkDir.&lc_TempTyp;
   WHERE ( &lc_TempTyp..cFlag="û" .AND. ;
         &lcTypes..cTypeCode = &lc_TempTyp..ctypeCode ) .AND. lfTherm();
   INTO DBF &gcWorkDir.&lc_NewAcc
  *** terminating global Function thermometer
  IF lnTotalRec <> lnSelected 
    FOR lnSelected=lnSelected TO lnTotalRec
      =gfThermo(lnTotalRec,lnSelected,'Selecting types..',' ')
    ENDFOR
  ENDIF
  *** message "xxx Type(s) are added" with options (Add,Cancel)
  *** if the user choice is cancel; teminating the the Function
  lcParam=ALLTRIM(STR( _TALLY))+IIF(_TALLY=1,"| is","|s are")
  IF gfModalGen("TRM02105B02008","DIALOG",lcParam) = 2
    RETURN
  ENDIF
  *** selecting temp. file "lc_TempDst"
  SELECT (lc_TempDst)
  *** Cheking that the index is already created
  IF SYS(22)<>"TypeCode"
  *** indexing the file for further seek
  INDEX ON cTypeCode TAG TypeCode
  ENDIF
  SET ORDER TO TAG TypeCode
  *** If there is lines from before
  IF lnGrpLins > 0
    *** message "Adding New Records" with options" :
    *** replace,merge,cancel  
    lnOption = gfModalGen("TRM02104B02012","DIALOG")
    DO CASE
      *** Replace option is selected
      CASE lnOption = 1
        *** selecting temp. file "lc_TempDst"
        SELECT (lc_TempDst)
        *** deleting old accounts
        DELETE ALL
        *** keeping track of deleted record count in the file "lc_TempDst"
        lnOldRecs  = RECCOUNT(lc_TempDst)
        *** counting the records in the file "lc_NewAcc"
        lnTotalRec = RECCOUNT(lc_NewAcc )
        ***  appending from "lc_NewAcc" while calling the thermometer Function
        APPEND FROM &gcWorkDir.&lc_NewAcc ;
               FOR gfThermo(lnTotalRec,RECNO(lc_TempDst)-lnOldRecs,;
                   'Adding type..',cTypeCode)
        IF lnTotalRec <> RECNO(lc_TempDst)-lnOldRecs
          FOR lnTemp=RECNO(lc_TempDst)-lnOldRecs TO lnTotalRec
            =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
          ENDFOR
        ENDIF
        *** updating "lnGrpLins" (group lines) variable  
        lnGrpLins = RECCOUNT(lc_NewAcc)
      *** Merge option was selected
      CASE lnOption = 2
        *** selecting temp. file "lc_TempDst"
        SELECT (lc_TempDst)
        *** Cheking that the index is already created
        IF SYS(22)<>"TypeCode"
           *** indexing the file for further seek
           INDEX ON cTypeCode TAG TypeCode
        ENDIF
        SET ORDER TO TAG TypeCode
        *** reindexing is issued because the module will be used separtely
        REINDEX 
        *** saving group lines count before merge
        lnOldGrLin = lnGrpLins
        *** selecting temp. file "lc_NewAcc"
        SELECT (lc_NewAcc)
        *** counting total records to be merged
        lnTotalRec = RECCOUNT()
        *** scaning the file "lc_NewAcc"
        SCAN 
          *** checking that the account code is not present in the
          *** group lines temp. file "lc_TempDst"
          IF !SEEK(&lc_NewAcc..cTypeCode,lc_TempDst)
            *** themometer Function
            =gfThermo(lnTotalRec,RECNO(lc_NewAcc),'Adding type..',cTypeCode)
            *** copying record data to memory variable
            lnTemp=RECNO(lc_NewAcc)
            SCATTER MEMVAR MEMO
            *** selecting temp. file for group lines "lc_TempDst"
            SELECT (lc_TempDst)
            *** adding blank record
            APPEND BLANK
            *** copying record data from memory variable
            *** to the temp. file "lc_TempDst"
            GATHER MEMVAR MEMO
            *** incrementing group lines variable "lnGrpLins"
            lnGrpLins = lnGrpLins + 1
            *** selecting temp. file "lc_NewAcc"
            SELECT (lc_NewAcc) 
          ENDIF
        ENDSCAN
        IF lnTotalRec <> RECNO(lc_NewAcc)
          FOR lnTemp=lnTemp TO lnTotalRec
            =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
          ENDFOR
        ENDIF
      CASE lnOption = 3
        RETURN         
    ENDCASE
  *** There is no lines from before
  ELSE
    *** selecting temp. file "lc_TempDst"
    *** in this case the group previously has no accounts
    *** so total selected lines will be added
    SELECT (lc_TempDst)
    *** counting total records in the temp. file "lc_NewAcc"
    lnTotalRec = RECCOUNT(lc_NewAcc)
    *** appending from temp. file "lc_NewAcc" to temp. file "lc_TempDst"
    APPEND FROM &gcWorkDir.&lc_NewAcc ;
           FOR gfThermo(lnTotalRec,RECNO(lc_TempDst),'Adding Type..',;
           &lc_NewAcc..cTypeCode)
    IF lnTotalRec <> RECNO(lc_TempDst)
      FOR lnTemp=RECNO(lc_TempDst) TO lnTotalRec
        =gfThermo(lnTotalRec,lnTemp,'Adding account..',' ')
      ENDFOR
    ENDIF
    *** updating group lines variable "lnGrpLins"
    lnGrpLins = RECCOUNT(lc_NewAcc)  
  ENDIF  
  *** terminating adding lines to the group session; by terminating the read
  *** and closing opend files
  CLEAR READ
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
=gfThermo(lnTotalRec,lnSelected,IIF(llFlag,'Selecting accounts..','Selecting types..'),' ')
