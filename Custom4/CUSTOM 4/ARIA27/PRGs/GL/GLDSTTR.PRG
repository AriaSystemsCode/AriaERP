*:************************************************************************
*:
*: Procedure file: GLDSTTR.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Renee Ezzat Ashamalla
*:      Copyright (c) 
*:  Last modified: 04/06/1994
*:
*:  Procs & Fncts
*:    PROCEDURE lpShow       *** local READ SHOW clause                ***
*:    PROCEDURE lpSavScr     *** local <  Save  > procedure            ***
*:    PROCEDURE lpDelScr     *** local < Delete > procedure            ***
*:    FUNCTION  lfTotals     *** called from Select_SQL command        ***
*:    FUNCTION  lfvData_2    *** Valid function of laData[2] get field ***
*:    FUNCTION  lswDstDet    *** When  function of list lsDstDet       ***
*:    FUNCTION  lfvAccCode   *** Valid function of cAcctCode get field ***
*:    FUNCTION  lfvAdtAmnt   *** Valid function of lnAmount  get field ***
*:    FUNCTION  lfvNew       *** Valid function of push button pbNew   ***
*:    FUNCTION  lfvRem       *** Valid function of push button pbRem   ***
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*B600379,1 Reham On 06/05/95
*B600379,1 1- Control the enabling & disabling of the <NEW> button in
*B600379,1    the different modes.
*B600379,1 2- Modify the interface of the list title under windows.
*B600379,1 3- Modify the enabling & disabling for the account browse 
*B600379,1    invisable button.

*B600385,1 Reham On 06/07/95
*B600385,1 In the key field, If coming from the browse to the select mode
*B600385,1 point to the current object which is the key field.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B602247,1 KAM 04/09/2000 add lines of code in screen to allow browse screen 
*B602247,1                 in account code object the adding in WHEN OF account code object
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField
DECLARE laKeyField [2,4]  
DECLARE laAutDtFld [1,4]       && holds the file structure of 'GLAUTDT' file
 
laKeyField[1,1]    = 'laData[1]'
laKeyField[1,2]    = .F.
laKeyField[1,3]    = 'TYPECODE'
laKeyField[1,4]    = 1
laKeyField[2,1]    = 'laData[2]'
laKeyField[2,2]    = .T.
laKeyField[2,3]    = 'TYPECODE' 
laKeyField[2,4]    = 2

*** Disable Some options of the Control Panel  
laDefProc[7]       = .F.     && Delete procedure(lpDelScr) 
laDefProc[9]       = .F.     && Save procedure(lpSavScr)

*** Variables declaration 
lsDstDet   = 1       && list variable ( pointer )
lcFieldst  = ''      && list field string
lc_TmpDst  = ''      && temporary file name variable

lcAcctCode = REPLICATE ("0",lnAcsSegSz)
lcOldAcct  = ''      && old account code
lcAccnlDes = ''      && corresponds to field GLAUTDT.cAccnlDes
lcCodeType = ''      && variable to hold the current account type 
lcAutDtTg  = ''      && Tag name of GLAUTDT upon entry.      
lcAutDtEx  = ''      && Tag expression of GLAUTDT current tag 
lcDStamp   = ''      && Audit information of current record  
lnTmpRcCnt = 0       && number of records in th list
lnOldDsRec = 0       && the previously selected record (position) 
lnThermRec = 0       && Thermometer counter

lnOldAmnt  = 0       && old percentage amount 
lnAmount   = 0       && corresponds to field GLAUTDT.nAmount
lnDstTotal = 0       && variable to hold total of distribution %s.
lnAutDtFld = 0          && number of fields of 'GLAUTDT' file

llFromBton = .F.
llBrowse   = .F.

*B600379,1 Reham On 06/05/95 Add this flag to know if there is any 
*B600379,1 detail line without an account.
llNewDet   = .F.

*** Variables in screen objects file
lcTDfRef   = ""      && 'On'  
lcTDfDesc  = ""      && 'Created by'
lcTDstAmnt = ""      && 'distributed amounts'
lcTDstTr   = ""      && 'distribution transaction|distribution transaction'
lcTTotals  = ""      && 'Total of all entries'

*** Screen display variable :
lcTTotal   = ""      && 'Total :'  

*** Variables hold the status of objects. ***
lcAct_Stat = ""
lcPer_Stat = ""
lcNew_Stat = ""
lcRem_Stat = ""

IF !gfSetup()    
  RETURN
ENDIF  

lcUserName = gcUserName
*lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')
lcCurDate  = IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                 LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))

SELECT GLTYPES

GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting       = .T.  
  RETURN 
ENDIF

IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laData[1]       = 'D'

  *** Create a name for the temporary file 
  lc_TmpDst        = gfTempName()
  
  *** Create the temporary file with the appropriate fields , 
  ***(in this case all fields of file "GLAUTDT" + 2 more fields
  *** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)
  SELECT GLAUTDT
  
  lcAutDtTg       = SYS(22)
  
  SET ORDER TO TAG TYPCODACC
  
  *** Current tag expression is cAutType + cAutCode + cAcctCode
  lcAutDtEx       = SYS(14,VAL(SYS(21)))
  
  =AFIELDS(laAutDtFld)
  lnAutDtFld = ALEN(laAutDtFld,1)
  DIMENSION laAutDtFld[lnAutDtFld+2,4]

  laAutDtFld[lnAutDtFld+1,1] = 'nRecNo'
  laAutDtFld[lnAutDtFld+1,2] = 'N'
  laAutDtFld[lnAutDtFld+1,3] = 10
  laAutDtFld[lnAutDtFld+1,4] = 0

  laAutDtFld[lnAutDtFld+2,1] = 'cStatus'
  laAutDtFld[lnAutDtFld+2,2] = 'C'  
  laAutDtFld[lnAutDtFld+2,3] = 1
  laAutDtFld[lnAutDtFld+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_TmpDst;
      FROM ARRAY laAutDtFld

  *** Prepare lcFieldSt for the list
  SELECT (lc_TmpDst)
  
  lcFieldSt     = "SUBSTR(&lc_TmpDst..cAcctCode,1,lnAcsSegSz)+' '+"+;
                     "SUBSTR(LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpDst..cAcctCode,"+;
                     "GLACCHAR.cAcctCode,'ACCTCODE'),1,IIF(_DOS,57-lnAcsSegSz,48-lnAcsSegSz))+"+;
                     "+SPACE(9)+STR(&lc_TmpDst..nAmount,6,2)"                
ENDIF

SELECT GLAUTHD
SET FILTER TO

*** Filter on distribution transactions records only
SET FILTER TO cAutType="D"
*B600379,1 Reham On 06/05/95 
*B600379,1 Change the command from "LOCATE" to the next command to prevent
*B600379,1 changing the record no. if leaving the screen to another one &
*B600379,1 back again in the GLAUTHD
LOCATE FOR CAUTCODE = laData[2]

IF laScrMode[1] .OR. laScrMode[2]
  STORE "DISABLE" TO lcAct_Stat,lcPer_Stat,lcNew_Stat,lcRem_Stat
ELSE
  lcAct_Stat = IIF(lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
  lcPer_Stat = IIF(lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
  
  *B600379,1 Reham On 06/05/95 Add the flag "llNewDet" to the condtion
  *B600379,1 to know if there is detail line without an acc.
  lcNew_Stat = IIF(lnDstTotal = 100 .OR. llNewDet, "DISABLE" , "ENABLE")
  lcRem_Stat = IIF(lnTmpRcCnt = 0   , "DISABLE" , "ENABLE")
ENDIF

*E300683,5 Call *.SPR from screens directory
* DO GLDstTr.SPR 
DO (gcScrDir + gcWinAppl + '\GLDstTr.SPR')
*E300683,5 end   
*** Clean Up
SELECT GLAUTHD

SET FILTER TO  
IF glQuitting
  *** reset tag of GLAUTDT
  SELECT GLAUTDT
  IF !EMPTY(lcAutDtTg)
    SET ORDER TO TAG (lcAutDtTg)
  ELSE
    SET ORDER TO
  ENDIF
      
  *** Close open temporary files.  
  IF USED(lc_TmpDst)
    USE IN ALIAS(lc_TmpDst)
  ENDIF
  ERASE &gcWorkDir.&lc_TmpDst..dbf  
ENDIF 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

laData[1] = 'D'

DO CASE  
  *** "Select" mode (laScrMode[1]=.T.)
  CASE laScrMode[1] 
    SELECT (lc_TmpDst)
    lsDstDet   = 1
    SHOW GET lsDstDet    ENABLE

    *** Delete old data ( if any )
    ZAP
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsDstDet        
    ON ERROR &lcErrSett.

    *** Initialize variables for display
    lnTmpRcCnt = 0       && No nonempty records in the temporary file
    lnOldDsRec = 1

    lcAcctCode = REPLICATE ("0",lnAcsSegSz)
    lcAccnlDes = SPACE(60)
    lcOldAcct  = ''

    lnAmount   = 0
    lnOldAmnt  = 0
    lnDstTotal = 0

    SHOW GET lcAcctCode DISABLE
    
    *B600379,1 Reham On 06/05/95 Control the enabling & disabling of the
    *B600379,1 account browse invisable button.
    SHOW GET ibActBrow  DISABLE
    
    SHOW GET lnAmount   DISABLE

    *** Controls
    SHOW GET pbNew      DISABLE 
    SHOW GET pbRem      DISABLE 
    
  *** "View" mode (laScrMode[2]=.T.), or "Edit" mode (laScrMode[3]=.T.)
  CASE laScrMode[2].OR.laScrMode[3] 
                
    llFirstRec     = .T.

    SELECT (lc_TmpDst)
    lsDstDet = 1
    
    SHOW GET lsDstDet         

    lcDStamp       = IIF(laScrMode[2], GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time,lcDStamp)
        
    IF laScrMode[2] .OR. lcDStamp <> GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time
      SELECT GLAUTDT  

      lnDstTotal   = 0
      SELECT *,RECNO() AS 'nRecNo',	"S" AS 'cStatus';
             FROM &gcDataDir.GLAUTDT;
             INTO DBF &gcWorkDir.&lc_TmpDst;
             WHERE &lcAutDtEx. ="D"+laData[2];
             .AND. lfTotals()
    ENDIF
    
    SELECT (lc_TmpDst)

    *** Get the type of accounts
    IF laScrMode[3]
      lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,;
                      &lc_TmpDst..cAcctCode,;
                      GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")    
    ENDIF                             
   
    *** Get the number of records currently in the temporary file
    lnTmpRcCnt = RECCOUNT(lc_TmpDst)

    *** Select the previously selected record,( lnOldDsRec)
    lsDstDet   = IIF(laScrMode[3],lnOldDsRec,1)
    lnOldDsRec = lsDstDet
    
    IF RECCOUNT(lc_TmpDst) > 0
      GO lnOldDsRec
    ENDIF   

    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** until there is a total number of distribution %s =100 ),
    *** There seems to be no reason for checking for the number of
    *** Records in the file,or to initialize variables to spaces,hence,
    lcAcctCode = cAcctCode
    lcAccnlDes = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpDst..cAcctCode,;
                        GLACCHAR.cAcctCode,'ACCTCODE')
    lnAmount   = nAmount
      
    *** Prepare screen objects
    lcObjState = IIF(laScrMode[3],"ENABLE","DISABLE")
    SHOW GET lcAcctCode  &lcObjState.
    SHOW GET ibActBrow   &lcObjState.
    SHOW GET lnAmount    &lcObjState.
    SHOW GET lcAccnlDes  &lcObjState.

    lcObjState = IIF(laScrMode[3] .AND. !EOF(lc_TmpDst),;
                         "ENABLE","DISABLE")
    SHOW GET pbRem       &lcObjState.
    
    *** Just in case the New button is enabled in EDIT mode,
    *** disable it in VIEW mode 
    IF laScrMode[2]
      SHOW GET pbNew     DISABLE
    ENDIF   

    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SELECT (lc_TmpDst)
    SHOW GET lsDstDet ENABLE
    ON ERROR &lcErrSett.
  
  *** "Add" mode (laScrMode[4]=.T.) 
  CASE laScrMode[4]
    *** Prepare defaults
    *** Remember that lcCurDate's length depends upon the century setting.
    lcDfRef    = lcTDfRef+' '+lcCurDate
    laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    laData[4]  = SUBSTR(lcTDfDesc+' '+lcUsername,1,;     
                         FSIZE('cAutDes','GLAUTHD'))
    
    SHOW GET pbNew    ENABLE 
    SHOW GET lsDstDet
ENDCASE 

SELECT GLAUTHD   
 
*!**************************************************************************
*!
*!      Function: lfTotals
*!
*!**************************************************************************
*    This function gets the total distribution percentages,called
*    while SELECT_SQL command is executed.
*
FUNCTION lfTotals

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec       = .F.
ELSE  
  lnDstTotal       = lnDstTotal + nAmount
ENDIF 

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*  VALID function for the get field "laData[2]" corresponding
*  to field GLAUTHD.cAutCode that constitutes a part of a 
*  two field primary key : GLAUTHD.cAutType + GLAUTHD.cAutCode   
*
FUNCTION lfvData_2

IF llBrowse .OR. !EMPTY(laData[2])
  IF llBrowse
    =gfSeekRec()
    *B600385,1 Reham On 06/07/95 Added to point to the current object.
    IF laScrMode[1]
      _CUROBJ = OBJNUM(laData[2])
    ENDIF
  ELSE
    laData[2]  = ALLTRIM(laData[2])
    laData[2]  = IIF(ISDIGIT(LEFT(laData[2],1)),;
                     PADL(laData[2], FSIZE('cAutCode','GLAUTHD'),'0'),;
                     PADR(laData[2], FSIZE('cAutCode','GLAUTHD'),SPACE(1)))
    SHOW GET laData[2]
    
    IF LASTKEY() = 13
      IF LEFT(laData[2],1) = '?'
        laData[1]  = '?'
        laData[2]  = SPACE(8)
      ENDIF
      =gfSeekRec()
      *B600385,1 Reham On 06/07/95 Added to point to the current object.
      IF laScrMode[1]
        _CUROBJ = OBJNUM(laData[2])
      ENDIF
    ENDIF
  ENDIF
ENDIF

llBrowse   = .F.
laData[1]  = 'D'

*!**************************************************************************
*!
*!      Function: lswDstDet
*!
*!**************************************************************************
*    WHEN function for the list "lsDstDet" 
*
FUNCTION lswDstDet

*** IF There are no records in the list,return
*** prohibit selection from list.
IF  lnTmpRcCnt=0
  _CUROBJ          = OBJNUM(pbNew) 
  RETURN .F.
ENDIF  

*** If current record has no account code entry,
*** prohibit selection from list.
IF VAL(STRTRAN(lcAcctCode,'-',''))=0 
  RETURN .F.
ENDIF
 
SELECT (lc_TmpDst)

*** Refresh get fields with current contents
lcAcctCode = cAcctCode
lcAccnlDes = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpDst..cAcctCode,;
                    GLACCHAR.cAcctCode,'ACCTCODE')
lnAmount   = nAmount

SHOW GET lcAcctCode 
SHOW GET lcAccnlDes 
SHOW GET lnAmount 

*** Save the currently selected record position
lnOldDsRec = lsDstDet

SELECT GLAUTHD
  
*!**************************************************************************
*!
*!      Function: lfwActBrow
*!
*!**************************************************************************
* When func. for browse icon...
*
FUNCTION lfwActBrow

DO CASE
  CASE LASTKEY() = 9 .OR. LASTKEY() = 24
    _CUROBJ = OBJNUM(lcAcctCode)
  CASE LASTKEY() = 15 .OR. LASTKEY() = 5
    _CUROBJ = OBJNUM(lsDstDet)
ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*    VALID function for get field "lcAcctCode" 
*
FUNCTION lfvAccCode
PRIVATE lcCodeT

IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND. !ISDIGIT(LTRIM(lcAcctCode))
  =gfModalGen("TRM02061B00000","Dialog")
  lcAcctCode = lcOldAcct  
  SHOW GET lcAcctCode
  _CUROBJ    = OBJNUM(lcAcctCode)
  RETURN
ENDIF

IF lcAcctCode = lcOldAcct 
  *** No need for validation
  RETURN
ELSE
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry
  IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND.VAL(STRTRAN(lcAcctCode,'-','')) = 0  
    lcAcctCode = lcOldAcct  
    SHOW GET lcAcctCode 
  ELSE
    lcCodeT    = ''
    *** If there is only one record in the temporary file,
    *** The Account Type may be changed.
    *** This condition applies for the following cases :
    *** a. A NEW entry ( Add mode, or Edit mode) in an empty list.
    *** b. Removing all records except one.
    *** c. Removing all records ( handled from NEW )
    IF lnTmpRcCnt = 1
      lcCodeType = "A"
    ENDIF
  
    IF lfVldAccnt(lcCodeType,"C","L",.T.,@lcAccnlDes,@lcCodeT,"") 
      IF lcCodeType = "A"
        lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
      ENDIF    

      SELECT (lc_TmpDst)     
       
      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       
 
      lcStatus  = SUBSTR("MAM",AT(cStatus,"MAS"),1)
 
      REPLACE cAcctcode WITH lcAcctCode ,;
              cStatus   WITH lcStatus

      *** Refresh objects
      SHOW GET lsDstDet
      SHOW GET lcAccnlDes
      =lfRefresh() 
      
      SELECT GLAUTHD   
      
      *** Enable disabled objects
      *** Since zero entries are allowed in the amounts field,
      *** New button may be enabled as soon as a valid account
      *** is entered unless total is already = 100.
      IF lnDstTotal = 100
        _CUROBJ = OBJNUM(pbSav)    
      ELSE
        SHOW GET pbNew   ENABLE
        *B600379,1 Reham On 06/05/95 Add this flag to know if there is any 
        *B600379,1 detail line without an account.
        llNewDet   = .F.
      ENDIF
      SHOW GET lnAmount  ENABLE
    ELSE  
      lcAcctCode = lcOldAcct
      SHOW GET lcAcctCode
      _CUROBJ    = OBJNUM(lcAcctCode)
    ENDIF  
    CLEAR TYPEAHEAD
  ENDIF  
ENDIF  

*!**************************************************************************
*!
*!      Function:  lfvAdtAmnt
*!
*!**************************************************************************
*    VALID function for get field "lnAmount" 
*
FUNCTION lfvAdtAmnt

IF lnAmount = lnOldAmnt
  RETURN
ENDIF

*** Reject negative entries
IF lnAmount < 0
  =gfModalGen("TRM02036B00000","DIALOG")
  lnAmount   = lnOldAmnt
  SHOW GET lnAmount
  _CUROBJ    = OBJNUM(lnAmount)
ELSE 

  SELECT (lc_TmpDst)

  *** If the user inputs an entry in this field,
  IF lnAmount  > 0  .AND. (lnDstTotal-nAmount+lnAmount) > 100
    *** If this entry caused the total of the distribution percentages
    *** to exceed 100 %, do not accept the entry,and present a message
    =gfModalGen("TRM02017B00000","DIALOG",lcTDstAmnt)
    lnAmount = lnOldAmnt
    SHOW GET lnAmount
    *** Return to field
    _CUROBJ  = OBJNUM(lnAmount)    
  ELSE  
    lnDstTotal = lnDstTotal-nAmount+lnAmount
    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount WITH lnAmount ;
            cStatus WITH lcStatus
    
    *** Adjust controls
    IF VAL(STRTRAN(lcAcctCode,'-','')) <> 0
      lcObjState = IIF(lnDstTotal=100,"DISABLE","ENABLE")
      *B600379,1 Reham On 06/05/95
      *B600379,1 Set the flag to know the <NEW> button status.
      llNewDet   = IIF(lnDstTotal=100,.T.,.F.)
      SHOW GET pbNew &lcObjState.
    ENDIF
    SHOW GET lsDstDet
    =lfRefresh()
 
    SELECT GLAUTHD
  
    lcNextObj  = IIF(lnDstTotal=100,"pbSav","pbNew")
    _CUROBJ    = OBJNUM(&lcNextObj.)
  ENDIF
ENDIF
   
*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*    VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

SELECT (lc_TmpDst)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH laData[1],;
          cAutCode WITH laData[2],;   
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpDst.;
              (cAutType,cAutCode,cStatus);
         VALUES (laData[1],laData[2],"A")      
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info()

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
lcAcctCode = REPLICATE ("0",lnAcsSegSz)
lcAccnlDes = SPACE(60)
lnAmount   = 0

*** Increase number of records in temporary file
lnTmpRcCnt = lnTmpRcCnt + 1

*** Select the new record from the list
lsDstDet   = lnTmpRcCnt

SHOW GET lcAcctCode ENABLE
SHOW GET ibActBrow  ENABLE
SHOW GET lcAccnlDes
SHOW GET lnAmount   ENABLE

IF lnTmpRcCnt = 1
  SHOW GET pbRem    ENABLE
ENDIF
*** Disable New button until a valid account is ebtered
SHOW GET pbNew      DISABLE

*B600379,1 Reham On 06/05/95 Add this flag to know that there is detail
*B600379,1 line added but its account is not added yet.
llNewDet   = .T.

SHOW GET lnAmount   DISABLE 

*** Do not forget to refresh the list
SHOW GET lsDstDet  

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
_CUROBJ = OBJNUM(lcAcctCode)

*** Always return to the original work aria
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*    VALID function for push button "Remove" (pbRem).
*  
FUNCTION lfvRem
  
*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1

  SELECT (lc_TmpDst)  

  *** If the record is previuosly modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Decrement number of records in list
  lnTmpRcCnt = lnTmpRcCnt - 1

  *** Adjust total distribution
  lnDstTotal = lnDstTotal - nAmount

  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  SKIP
  IF EOF(lc_TmpDst)
    GO TOP
  ENDIF  
  lsDstDet   = IIF(lnTmpRcCnt=0,1,IIF(lsDstDet=lnTmpRcCnt+1,1,lsDstDet))
  lnOldDsRec = lsDstDet

  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  lcAcctCode = IIF(lnTmpRcCnt=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
  lcAccnlDes = IIF(lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                   &lc_TmpDst..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
  lnAmount   = IIF(lnTmpRcCnt=0,0,nAmount)                                      

  *** Adjust controls
  lcObjState = IIF(lnTmpRcCnt=0,"DISABLE","ENABLE")

  SHOW GET lcAcctCode &lcObjState.  
  SHOW GET ibActBrow  &lcObjState.       
  SHOW GET lcAccnlDes 

  SHOW GET lnAmount   &lcObjState.
  SHOW GET pbRem      &lcObjState.

  lcObjState = IIF(lnDstTotal<100,"ENABLE","DISABLE")
  *B600379,1 Reham On 06/05/95
  *B600379,1 Set flag to know the new button status.
  llNewDet   = IIF(lnDstTotal<100,.F.,.T.)
  SHOW GET pbNew      &lcObjState.

  *** Update list contents 
  SHOW GET lsDstDet
  =lfRefresh()

  _CUROBJ = OBJNUM(lsDstDet)

  SELECT GLAUTHD
ENDIF

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!*******************************************************************************
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr 

DO CASE
  *** If the file is empty,i.e. there are no recurring transactions,
  *** "Cannot save an empty transaction" 
  CASE (lnTmpRcCnt = 0 )
    =gfModalGen("TRM02035B00000","Dialog",lcTDstTr)
        
    llCSave   = .F.      && Do not change mode
    _CUROBJ   = OBJNUM(pbNew)
    RETURN

  CASE VAL(STRTRAN(lcAcctCode,'-',''))=0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave   = .F.
    _CUROBJ   = OBJNUM(lcAcctCode)
    RETURN

   *** Check if total distribution percentage=100
   *** If not,display an appropriate message and exit
  CASE lnDstTotal <>100 
    =gfModalGen("TRM02018B00000","DIALOG",lcTTotals)   
    llCSave   = .F.   
    RETURN

  OTHERWISE
    *** Attempt to lock files
    *** If 'A'dding a new header, attempt to lock 'GLAUTHD', 'GLAUTDT'
    *** files.
    *** If 'E'diting a record, attempt to lock the current record in 
    *** GLAUTHD file (the edited record), and  'GLAUTDT' file.
    *** If adding a new record,append a blank one
    
    *** If adding a new record,append a blank one
    IF laScrMode[4]
      APPEND BLANK
      =gfObj_Lock(.T.)
    ENDIF

    *** Store laData values in the current record
    GATHER FROM laData fields &lcScFields.

    *** Now save the data in the temporary file using the following global
    *** function which performs correct translation from the temporary
    *** file lc_TmpDst,and the main file GLAUTDT
    =gfTmp2Mast("GLAUTDT",lc_TmpDst,;
                'Saving automatic distribution '+laData[2]+' ...')  

    SELECT GLAUTHD
    IF laScrMode[4]
      =gfObj_Lock(.F.)
    ENDIF

   *** Unlock any locked files (if any)
*   IF laScrMode[3]
*     =gfRLock(RECNO(),.F.) 
*     =gfFLock('GLAUTDT',.F.)
*   ELSE
*     =gfFLock('GLAUTHD,GLAUTDT',.F.)        
*   ENDIF
       
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!*******************************************************************************
*    This procedure handles deletion,instead of the global procedure.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete procedure.
*
PROCEDURE lpDelScr 

SELECT GLAUTHD

*** Check if this record is already deleted by another user from
*** a different station. If it is, the record pointer is no longer
*** on the viewed record which is now actually out of scope if 
*** SET('DELETED')='ON'
IF GLAUTHD.cAutType + GLAUTHD.cAutCode <> laData[1]+laData[2] 
  *** If the record is already deleted, present the following message,
  *** and go to 'Select' mode
  *** Message : "
  
  =gfModalGen("TRM00095B00000","ALERT")
  
  *** Go back to 'Select' mode
  laScrMode        = ""
  laScrMode[1]     = .T.
  RETURN
ENDIF  

lnTotRec           = RECCOUNT(lc_TmpDst)+1
lnThermRec         = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpDst is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK

REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
        FOR &lcAutDtEx. = laData[1]+laData[2] .AND. ;
            lfThermo('Deleting automatic distribution '+laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

IF lnThermRec < lnTotRec
  =gfThermo(lnTotRec,lnTotRec,;
            'Deleting automatic distribution '+laData[2]+' ...','')
ENDIF

*** Return to "SELECT" mode
laScrMode    = .F.
laScrMode[1] = .T.
SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function : lfThermo
*!
*!*******************************************************************************
*  This function calls global function gfThermo (thermometer)
*  It takes as a parameter a counter to be incremented and tested 
*  at every call. The thermometer is called at increments of 13
*  instead of 1 for faster processing.
*
FUNCTION lfThermo
PARAMETERS lcThermStr

lnThermRec         = lnThermRec + 1
