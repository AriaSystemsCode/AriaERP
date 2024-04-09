*:************************************************************************
*:
*: Procedure file: GLTMPTR.PRG
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
*:    FUNCTION  lfvType      *** Valid function of radio button rbType ***
*:    FUNCTION  lswTrnDet    *** When  function of list lsTrnDet       ***
*:    FUNCTION  lfvAccCode   *** Valid function of cAcctCode get field ***
*:    FUNCTION  lfvDebit     *** Valid function of lnDebit   get field ***
*:    FUNCTION  lfvCredit    *** Valid function of lnCredit  get field ***
*:    FUNCTION  lfvNew       *** Valid function of push button pbNew   ***
*:    FUNCTION  lfvRem       *** Valid function of push button pbRem   ***
*:    FUNCTION  lfvAdj       *** Valid function of push button pbAdj   ***
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*B600378,1 Reham On 06/03/95 
*B600378,1 - Restore the right value of type radio button.
*B600378,1 Reham On 06/05/95 
*B600378,1 - Control the enabling & disabling of the type radio button 
*B600378,1   in all available modes.
*B600378,1 _ Control the enabling & disabling of the new button in all
*B600378,1   the cases & different modes.
*B600378,1 _ Add new flag "llNewDet" to know if there is a detail line
*B600378,1   without an account code.
*B600378,1 _ Modify the interface of the list title under windows in SCX.

*B600385,1 Reham On 06/07/95
*B600385,1 In the key field, If coming from the browse to the select mode
*B600385,1 point to the current object which is the key field.
*B600517,1 ORABY 07/11/1995 If the template type is precentage its code was not saved in the file
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B604980,1 ASH   07/25/2002 Fix the bug of disabled the NEW button in edit mode.
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField

DECLARE laKeyField [2,4]
DECLARE laAutDtFld [1,4]       && holds the file structure of 'GLAUTDT' file

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'TYPECODE'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'TYPECODE'
laKeyField[2,4] = 2

laDefProc[7] = .F.     && Use local Delete procedure 
laDefProc[9] = .F.     && Use local Save   procedure 

lsTrnDet   = 1         && list variable
rbType     = 1         && radio button 
lcAcctcode = REPLICATE("0",lnAcsSegSz) 
lcOldAcct  = ""        && old account code 
lcAccDesc  = ""        && corresponds to field GLAUTDT.cAccnlDes
lc_TmpTrDt = ""        && temporary file name 

lcCodeType = ""        && account code type 
lcFieldStr = ""        && field string for the list
lcTStamp   = ''        && Audit information of current record

lcAutDtTg  = ''        && Tag name of GLAUTDT upon entry.      
lcAutDtEx  = ''        && Tag expression of GLAUTDT current tag 

lnTmpRcCnt = 0         && number of records in th list
lnOldTmRec = 0         && previously selected record (position) 

lnOldDr    = 0         && old debit value
lnOldCr    = 0         && old credit value
lnDebit    = 0         && variable to accept debit  value
lnCredit   = 0         && variable to accept credit value
lnTotDr    = 0         && total debits
lnTotCr    = 0         && total credits
lnAutDtFld = 0         && number of fields of 'GLAUTDT' file
lnThermRec = 0         && Thermometer counter

llFromBton = .F. 
llBrowse   = .F.

*B600378,1 Reham On 06/05/95 This flag to know if there is any detail line
*B600378,1 added & its account is not entered yet.
llNewDet   = .F.

*** Text variables defined in Screen Objects file.
lcTDrPrc   = ""        && 'debits percentages'
lcTCrPrc   = ""        && 'credits percentages' 
lcTTmpTr   = ""        && 'template transaction|template transaction' 
lcTTmplt   = ""        && 'Template'
lcTDrCrPrc = ""        && 'Both total debit and credit percentages'
lcTDfRef   = ""        && 'On'
lcTDfDesc  = ""        && 'Created by'

*** Screen display variables :
lcTTotal   = ""        && 'Totals :'
lcTDebit   = ""        && 'Debit'
lcTCredit  = ""        && 'Credit'

*** Variables hold the stause of each button. ***
STORE "" TO lcAct_Stat , lcDeb_Stat , lcCrd_Stat , lcNew_Stat ,;
            lcRem_Stat , lcAdj_Stat , lcTyp_Stat

IF !gfSetup()    
  RETURN
ENDIF  

lcCurrSign = '$'        && currency sign
lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')
lcCurDate  = IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                 LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))

SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF
 
IF !WEXIST(gcBaseWind)
  
  SCATTER FIELDS &lcScFields TO laData BLANK
  laData[1]          = 'T'
  *B600517,1 Giving the Template type an intial value
   laData[5]  = "A"
  *** Create a name for the temporary file
  lc_TmpTrDt       = gfTempName()
  
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

  CREATE TABLE &gcWorkDir.&lc_TmpTrDt;
      FROM ARRAY laAutDtFld
      
  SELECT (lc_TmpTrDt)      

  *** Prepare lcFieldStr for the list 
  lcFieldStr       = "SUBSTR(&lc_TmpTrDt..cAcctcode,1,lnAcsSegSz)+' '+"+;
                     "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,"+;
                     "GLACCHAR.cAcctcode,'ACCTCODE'),1,IIF(_DOS,"+;
                     "40-lnAcsSegSz,31-lnAcsSegSz))+' '+"+;
                     "IIF(&lc_TmpTrDt..cDrOrCr='D',"+;
                     "STR(&lc_TmpTrDt..nAmount,15,2)+SPACE(12)+'0.00',"+;
                     "SPACE(11)+'0.00 '+STR(&lc_TmpTrDt..nAmount,15,2))" 
ENDIF

SELECT GLAUTHD
SET FILTER TO

*** Filter on template entries records only
SET FILTER TO CAUTTYPE = "T"
*B600378,1 Reham On 06/05/95 
*B600378,1 Change the command from "LOCATE" to the next command to prevent
*B600378,1 changing the record no. if leaving the screen to another one &
*B600378,1 back again in the GLAUTHD
LOCATE FOR CAUTCODE = laData[2]

*B600378,1 Reham On 06/03/95 Restore the right value of type radio button.
rbType  = IIF(laData[5]="A",1,2)

IF laScrMode[1] .OR. laScrMode[2]
  STORE "DISABLE" TO lcAct_Stat,lcDeb_Stat,lcCrd_Stat,lcNew_Stat,;
                     lcRem_Stat,lcAdj_Stat,lcTyp_Stat
ELSE
  lcAct_Stat = IIF(lnTmpRcCnt = 0,"DISABLE","ENABLE")
  lcDeb_Stat = IIF(lnTmpRcCnt = 0,"DISABLE","ENABLE")
  lcCrd_Stat = IIF(lnTmpRcCnt = 0,"DISABLE","ENABLE")
  
*  lcNew_Stat = IIF(VAL(STRTRAN(lcAcctCode,'-','')) = 0 .OR.;
                  (laData[5] <> "A" .AND. lnTotdr=100 .AND. lnTotcr=100),;
                  "DISABLE","ENABLE")
  *B600378,1 Reham On 06/05/95
  *B600378,1 Disable the <NEW> button in the following cases:_
  *B600378,1 1-Type was amount & total debit = total credit but <> 0
  *B600378,1 2-Type was percentage & both debit % & credit % equal 100
  *B600378,1 3-A new detail was added & its account not entered yet.
  IF lnTmpRcCnt > 0
    *B604980,1 ASH 07/25/2002 (Begin) Enable the New button even if TOTDR=TOTCR.
    *lcNew_Stat = IIF((rbType = 1 .AND. lnTotDr = lnTotCr .AND. lnTotDr <> 0) .OR. ;
                 (rbType = 2 .AND. lnTotDr = 100 .AND. lnTotCr = 100) .OR. ;
                 llNewDet , "DISABLE" , "ENABLE")
    lcNew_Stat = IIF((rbType = 2 .AND. lnTotDr = 100 .AND. lnTotCr = 100) .OR. llNewDet , "DISABLE" , "ENABLE")                 
    *B604980,1 ASH 07/25/2002 (End)
  ELSE
    lcNew_Stat = "ENABLE"
  ENDIF
  
  lcRem_Stat = IIF(lnTmpRcCnt = 0,"DISABLE","ENABLE")
  
  lcTyp_Stat = IIF(lnTmpRcCnt = 0,"ENABLE","DISABLE")
  
  IF rbType = 1 .AND. lnTotDr <> lnTotCr
    lcAdj_Stat = IIF(lnTmpRcCnt > 1 , "ENABLE","DISABLE")
  ELSE
    lcAdj_Stat = "DISABLE"
  ENDIF
ENDIF

*E300683,5 Call *.SPR from screens directory
* DO GLTmpTr.SPR 
DO (gcScrDir + gcWinAppl + '\GLTmpTr.SPR')
*E300683,5 end   
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
  IF USED(lc_TmpTrDt)
    USE IN ALIAS(lc_TmpTrDt)
  ENDIF
  ERASE &gcWorkDir.&lc_TmpTrDt..dbf  
ENDIF 
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

laData[1]          = 'T'

DO CASE
  *** "Select" mode (laScrMode[1]=.T.) 
  CASE laScrMode[1]
    SELECT (lc_TmpTrDt)
    lsTrnDet       = 1
	lcError        = ON("ERROR")
	ON ERROR lnDum = 1
    SHOW GET lsTrnDet   ENABLE
    *** Delete old data ( if any )
    ZAP
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
	SHOW GET lsTrnDet 
	ON ERROR &lcError
    *** Initialize variables for display
    *** Reset defaults
    laData[5]      = 'A'
    rbType         =  1   
    lcAcctcode     = REPLICATE("0",lnAcsSegSz) 
    lcAccDesc      = SPACE(60)   
    lcOldAcct      = ''
    lnOldTmRec     = 1    
    
    STORE 0 TO lnTmpRcCnt,lnDebit,lnCredit,lnTotdr,lnTotcr,lnOldDr,lnOldCr

    SHOW GET lcAcctCode DISABLE
    SHOW GET ibActBrow  DISABLE
    SHOW GET lnDebit    DISABLE 
    SHOW GET lnCredit   DISABLE 
    *B600378,1 Reham On 06/05/95 Disable the type radio button in select mode
    SHOW GET rbType     DISABLE

    *** Controls
    SHOW GET pbNew      DISABLE
    SHOW GET pbRem      DISABLE
    SHOW GET pbAdj      DISABLE 
    
  *** "View" mode (laScrMode[2]=.T.), or "Edit" mode (laScrMode[3]=.T.) 
  CASE laScrMode[2].OR.laScrMode[3]
    *** Header file object
    rbType         = IIF(laData[5]="A",1,2)
    lnDebit        = 0 
    lnCredit       = 0
    llFirstRec     = .T.
    
    *** The file is selected for lfTotals()   
    lcTStamp       = IIF(laScrMode[2], GLAUTHD.cAdd_User+;
                         DTOC(GLAUTHD.dAdd_Date)+;
                         GLAUTHD.cAdd_Time,lcTStamp)
        
    IF laScrMode[2] .OR. lcTStamp <> ;
           GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date)+GLAUTHD.cAdd_Time
      SELECT GLAUTDT
      lnTotDr      = 0
      lnTotCr      = 0
      SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus';
              FROM &gcDataDir.GLAUTDT ;
              INTO DBF &gcWorkDir.&lc_TmpTrDt;
              WHERE &lcAutDtEx. = 'T'+laData[2];
              .AND. lfTotals()
    ENDIF          
             
    SELECT  (lc_TmpTrDt)
      
    *** Get the type of accounts
    IF laScrMode[3]
      lcCodeType   = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                     GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
    ENDIF                             

    *** Get the number of records currently in the temporary file
    lnTmpRcCnt     = RECCOUNT(lc_TmpTrDt)

    *** Select the previously selected record,( lnOldTmRec)
    lsTrnDet       = IIF(laScrMode[3],lnOldTmRec,1)
    lnOldTmRec     = lsTrnDet        
    
   
    IF RECCOUNT(lc_TmpTrDt) > 0
      GO lnOldTmRec
    ENDIF   
    
    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** an empty recurring trasnsaction,there 
    *** seems to be no reason for checking for the number of
    *** records in the file,or to initialize variables to spaces,hence,
    
    lcAcctCode = cAcctcode
    lcAccDesc  = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,;
                        GLACCHAR.cAcctcode,'ACCTCODE')
    IF cDrOrCr ="D"
      lnDebit  = nAmount
    ELSE
      lnCredit = nAmount
    ENDIF
 
    *** Prepare screen objects
    lcObjState = IIF(laScrMode[3],"ENABLE","DISABLE")
    SHOW GET pbRem      &lcObjState.
   
    SHOW GET lcAcctCode &lcObjState.
    SHOW GET ibActBrow  &lcObjState.
    SHOW GET lnDebit    &lcObjState.
    SHOW GET lnCredit   &lcObjState.
    
    *** pbNew Control
    * ram lcObjState = IIF(laScrMode[2].OR.(laScrMode[3].AND.rbType=2),;
                       "DISABLE","ENABLE")
    * ram SHOW GET pbNew &lcObjState.
    
    *** If returning to 'View' mode from 'Edit' mode with the Adjust
    *** button enabled,we'd like to disable it.
    IF laScrMode[2]
      SHOW GET pbAdj  DISABLE
      *B600378,1 Reham On 06/05/95 Disable the type radio button in view mode.
      SHOW GET rbType DISABLE
      
      *B600378,1 Reham On 06/05/95 Disable the new button if view mode.
      SHOW GET pbNew  DISABLE
    ELSE
      IF lnTmpRcCnt = 0
        *B600378,1 Reham On 06/05/95 Enable the type radio button in 
        *B600378,1 edit mode if there is no template details.
        SHOW GET rbType ENABLE

        *B600378,1 Reham On 06/05/95 
        *B600378,1 Enable the new button if edit mode and no details added.
        SHOW GET pbNew ENABLE
      ELSE
        *B600378,1 Reham On 06/05/95 Disable the type radio button in 
        *B600378,1 edit mode if there is template details.
        SHOW GET rbType DISABLE
        
        *B600378,1 Reham On 06/05/95 
        *B600378,1 Disable the new button in the following cases:_
        *B600378,1 _Type was amount & both credit & debit amount are equal.
        *B600378,1 _Type was percentage & both total debit & credit = 100.
        *B600378,1 _New detail have just added & its account not entered yet.
        *B604980,1 ASH 07/25/2002 (Begin) Don't disable the new button if both credit & debit are equals, as the user can add another transactions.
        *IF (rbType = 1 .AND. lnTotDr = lnTotCr) .OR. ;
           (rbType = 2 .AND. lnTotDr = 100 .AND. lnTotCr = 100) .OR. ;
           llNewDet
        IF (rbType = 2 .AND. lnTotDr = 100 .AND. lnTotCr = 100) .OR. llNewDet           
        *B604980,1 ASH 07/25/2002 (End)
          SHOW GET pbNew DISABLE
        ELSE
          SHOW GET pbNew ENABLE
          *B604980,1 ASH 07/25/2002 (Begin) Enable the New button even if both credit & debit are equals.
          lcNew_Stat = 'ENABLE'
          *B604980,1 ASH 07/25/2002 (End)
        ENDIF
      ENDIF
    ENDIF 
   
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsTrnDet   ENABLE
    ON ERROR &lcErrSett.
   
  *** "Add" mode (laScrMode[4]=.T.)
  CASE laScrMode[4] 
    *** Prepare defaults :
    *** Remember that lcCurDate's length depends upon the century setting.
    lcDfRef        = lcTDfRef+' '+lcCurDate
    laData[3]      = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    laData[4]      = SUBSTR(lcTDfDesc+' '+lcUsername,1,;     
                     FSIZE('cAutDes','GLAUTHD'))

    SHOW GET pbNew    ENABLE
    
    *B600378,1 Reham On 06/05/95 Enable the type radio button in add mode.
    SHOW GET rbType   ENABLE
    SHOW GET lsTrnDet   
ENDCASE

SELECT GLAUTHD 

*!**************************************************************************
*!
*!      Function : lfTotals
*!
*!**************************************************************************
*
FUNCTION lfTotals

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec       = .F.
ELSE
  IF cDrOrCr ="D"
    lnTotdr        = lnTotdr  + nAmount
  ELSE
    lnTotcr        = lnTotcr  + nAmount
  ENDIF
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
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
    laData[2] = ALLTRIM(laData[2])
    laData[2] = IIF(ISDIGIT(LEFT(laData[2],1)),;
                    PADL(laData[2], FSIZE('cAutCode','GLAUTHD'),'0'),;
                    PADR(laData[2], FSIZE('cAutCode','GLAUTHD'),SPACE(1)))
    SHOW GET laData[2]
    IF LASTKEY() = 13
      IF LEFT(laData[2],1) = '?'
        laData[1] = '?'
        laData[2] = SPACE(8)
      ENDIF
      =gfSeekRec()
      *B600385,1 Reham On 06/07/95 Added to point to the current object.
      IF laScrMode[1]
        _CUROBJ = OBJNUM(laData[2])
      ENDIF
    ENDIF
  ENDIF
ENDIF
laData[1] = 'T'
llBrowse  = .F.

*!**************************************************************************
*!
*!      Function: lfvType
*!
*!**************************************************************************
*    VALID function for the radio button "rbType"
*
FUNCTION lfvType

laData[5] = SUBSTR("AP",rbType,1)
SHOW GET pbNew ENABLE
=lfRefresh()  

*!**************************************************************************
*!
*!      Function: lswTrnDet
*!
*!**************************************************************************
* WHEN function for the list "lsTrnDet" 
*
FUNCTION lswTrnDet

*** IF There are no records in the list,
*** prohibit selection from list.
IF  lnTmpRcCnt = 0
  _CUROBJ = OBJNUM(pbNew) 
  RETURN .F.
ENDIF

*** If current record has no account code entry,
*** prohibit selection from list.
IF VAL(STRTRAN(lcAcctCode,'-',''))=0 
  RETURN .F.
ENDIF

SELECT (lc_TmpTrDt)

*** Refresh get fields with current contents
lcAcctCode  = cAcctcode
lcAccDesc   = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,;
                     GLACCHAR.cAcctcode,'ACCTCODE')
IF cDrOrCr ="D"
  lnDebit   = nAmount
  lnCredit  = 0 
ELSE
  lnCredit  = nAmount
  lnDebit   = 0
ENDIF
  
SHOW GET lcAcctCode 
SHOW GET lcAccDesc
SHOW GET lnDebit    
SHOW GET lnCredit   
  
*** Save the currently selected record position
lnOldTmRec  = lsTrnDet

SELECT GLAUTHD

*!**************************************************************************
*!
*!      Function: lfwActBrow
*!
*!**************************************************************************
* When func. for browse icon...

FUNCTION lfwActBrow

DO CASE
  CASE LASTKEY() = 9 .OR. LASTKEY() = 24
    _CUROBJ = OBJNUM(lcacctcode)
  CASE LASTKEY() = 15 .OR. LASTKEY() = 5
    _CUROBJ = OBJNUM(lsTrnDet)
ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*  Valid function for the field lcAcctcode
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
  *** and now it is emptied,just ignore the entry.
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
      lcCodeType  = "A"
    ENDIF

    IF lfVldAccnt(lcCodeType,"C","L",.T.,@lcAccDesc,@lcCodeT,"")
      IF lcCodeType = "A"
        lcCodeType  = IIF(LEFT(lcCodeT,1)="Y","S","T")
      ENDIF    

      SELECT (lc_TmpTrDt)

      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       

      lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)

      *** Since both debit and credit fields may be left with
      *** zero values, use 'D'ebit as a default for a new
      *** entry
      REPLACE cAcctcode WITH lcAcctCode ,;
              cDrOrCr   WITH IIF(lnDebit = 0 .AND. lnCredit = 0,;
                                  'D',cDrOrCr),;
              cStatus   WITH lcStatus

      *** Refresh objects
      SHOW GET lsTrnDet
      SHOW GET lcAccDesc
      =lfRefresh()  
  
      SELECT GLAUTHD
       
      *** In this screen, both debit and credit fields may have
      *** zeroes in the same record,hence,pbNew may be enabled now..
      *** provided that, if in percent mode,totals do not exceed 100% 
      lcObjState = IIF(rbType=2.AND.lnTotCr=100.AND.lnTotDr=100,;
                       "DISABLE","ENABLE")
      llNewDet   = IIF(rbType=2.AND.lnTotCr=100.AND.lnTotDr=100,.T.,.F.)
      
      SHOW GET pbNew &lcObjState.
      IF lnDebit > 0 .OR. lnCredit > 0
        lcNextObj  = IIF(lnTotCr=lnTotDr.AND.((rbType=1.AND.lnTotCr<>0);
                      .OR.(rbType=2.AND.lnTotCr=100)),"pbSav","pbNew")
        _CUROBJ    = OBJNUM(&lcNextObj.)    
      ENDIF
      SHOW GET lnDebit  ENABLE
      SHOW GET lnCredit ENABLE
      IF rbType = 1 .AND. lnTotDr <> lnTotCr
        IF lnTmpRcCnt > 1
          lcObjState = "ENABLE"
        ELSE
          lcObjState = "DISABLE"
        ENDIF
      ELSE
        lcObjState = "DISABLE"
      ENDIF
*      lcObjState   = IIF((rbType=1.AND.lnTotDr<>lnTotCr) .OR. ;
                         lnTmpRcCnt > 1 ,"ENABLE","DISABLE")
      SHOW GET pbAdj     &lcObjState.
    ELSE
      lcAcctCode   = lcOldAcct
      SHOW GET  lcAcctCode   
      _CUROBJ      = OBJNUM(lcAcctCode)
    ENDIF
    CLEAR TYPEAHEAD
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDebit
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    lnTotDr field is used for totals
*   
FUNCTION lfvDebit

IF lnDebit <> lnOldDr
    
  *** Reject negative entries
  IF lnDebit< 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnDebit   = lnOldDr  
    SHOW GET lnDebit
    _CUROBJ   = OBJNUM(lnDebit)
  ELSE
    
    SELECT (lc_TmpTrDt)   

    *** In the case of percents ( laData[5]<>"A"
    *** If this entry causes the total total debit to exceed 100 %
    *** The user is presented with an appropriate message,and the 
    *** entry is rejected,returning the old value.  
    IF laData[5]<>"A" .AND. (lnTotDr - lnOldDr + lnDebit > 100)
      =gfModalGen("TRM02017B00000","DIALOG",lcTDrPrc)
      lnDebit  = lnOldDr
      SHOW GET lnDebit
      SELECT GLAUTHD
      _CUROBJ  = OBJNUM(lnDebit)
      RETURN 
    ENDIF   

    *** If the record has been previously saved as a credit,
    *** and now there is a debit entry,ignore the previuos 
    *** credit entry.
    IF cDrOrCr = "D"
      lnTotDr  = lnTotDr - nAmount + lnDebit
    ELSE
      lnTotCr  = lnTotCr - nAmount
      lnCredit = 0
      lnTotDr  = lnTotDr + lnDebit
      SHOW GET lnCredit
    ENDIF

    *** Assume a default of 'D'ebit if both lnDebit and lnCredit fields
    *** are equal to 0. Since at this point, lnCredit = 0, record
    *** the entry as 'D'ebit if it is greater or equal to 0.
    REPLACE nAmount WITH lnDebit,;
            cDrOrCr WITH "D",;
            cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  

    SELECT GLAUTHD
  
    *** Adjust controls
    SHOW GET lsTrnDet
    IF rbType = 1 .AND. lnTotDr <> lnTotCr
      IF lnTmpRcCnt > 1
        lcObjState = "ENABLE"
      ELSE
        lcObjState = "DISABLE"
      ENDIF
    ELSE
      lcObjState = "DISABLE"
    ENDIF
*    lcObjState     = IIF((rbType=1.AND.lnTotDr<>lnTotCr) .OR. ;
                         lnTmpRcCnt > 1 ,"ENABLE","DISABLE")
    SHOW GET pbAdj &lcObjState.
    =lfRefresh()
    IF lnDebit>0
      lcNextObj    = IIF(lnTotCr=lnTotDr.AND.;
           ((rbType=1.AND.lnTotCr<>0).OR.(rbType=2.AND.lnTotCr=100)),"pbSav","pbNew")
      _CUROBJ      = OBJNUM(&lcNextObj.)
    ENDIF
  ENDIF  
ENDIF 

*** If there is a valid account entry,
*** In case of %s,pbNew is disabled if total debits and credits =100%
*** otherwise it is enabled.
lcObjState = IIF( VAL(STRTRAN(lcAcctCode,'-',''))=0 .OR.;
                 (laData[5]<>"A".AND.lnTotdr=100.AND.lnTotcr=100),;
                 "DISABLE","ENABLE")
llNewDet   = IIF( VAL(STRTRAN(lcAcctCode,'-',''))=0 .OR.;
                 (laData[5]<>"A".AND.lnTotdr=100.AND.lnTotcr=100),;
                 .T.,.F.)
SHOW GET pbNew &lcObjState.

*!**************************************************************************
*!
*!      Function: lfvCredit
*!
*!**************************************************************************
* VALID function for get field "lnCredit".
* lnTotCr field is used for totals
*   
FUNCTION lfvCredit

IF lnCredit <> lnOldCr

  *** Reject negative entries
  IF lnCredit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnCredit   = lnOldCr  
    SHOW GET lnCredit
    _CUROBJ    = OBJNUM(lnCredit)
  ELSE 
    *lnLastKey = LASTKEY()

    SELECT (lc_TmpTrDt)
  
    *** In the case of percents ( laData[5]<>"A"
    *** If this entry causes the total total debit to exceed 100 %
    *** The user is presented with an appropriate message,and the 
    *** entry is rejected,returning the old value.  
    IF laData[5] <> "A" .AND. (lnTotCr - lnOldCr + lnCredit > 100)
      =gfModalGen("TRM02017B00000","DIALOG",lcTCrPrc)
      lnCredit  = lnOldCr
      SHOW GET lnCredit
      SELECT GLAUTHD
      _CUROBJ   = OBJNUM(lnCredit)
      RETURN 
    ENDIF   
  
    *** If the record has been previously saved as a debit,
    *** and now there is a credit entry,ignore the previuos 
    *** debit entry.
    IF cDrOrCr = "C"
      lnTotCr  = lnTotCr - nAmount + lnCredit
    ELSE
      lnTotDr  = lnTotDr - nAmount
      lnDebit  = 0
      lnTotCr  = lnTotCr + lnCredit
      SHOW GET lnDebit
    ENDIF
    
    *** Assume a default of 'D'ebit if both lnDebit and lnCredit fields
    *** are equal to 0
    REPLACE nAmount WITH lnCredit,;
            cDrOrCr WITH IIF(lnDebit=0.AND.lnCredit=0,"D","C");
            cStatus WITH  SUBSTR('MMA',AT(cStatus,'SMA'),1)    

    *** Adjust controls
    SHOW GET lsTrnDet
    IF rbType = 1 .AND. lnTotDr <> lnTotCr
      IF lnTmpRcCnt > 1
        lcObjState = "ENABLE"
      ELSE
        lcObjState = "DISABLE"
      ENDIF
    ELSE
      lcObjState = "DISABLE"
    ENDIF

*    lcObjState     = IIF((rbType=1.AND.lnTotDr<>lnTotCr) .OR. ;
                         lnTmpRcCnt > 1 ,"ENABLE","DISABLE")
    SHOW GET pbAdj &lcObjState.
    =lfRefresh()

    IF (rbType=1.AND.lnTotdr=lnTotcr.AND.lnTotCr<>0);
      .OR.(rbType=2.AND.lnTotdr=100.AND.lnTotcr=100)
      _CUROBJ  = OBJNUM(pbSav)
    ENDIF

    SELECT GLAUTHD
  ENDIF
ENDIF  

*** If there is a valid account entry,
*** In case of %s,pbNew is disabled if total debits and credits =100%
*** otherwise it is enabled.
lcObjState  = IIF( VAL(STRTRAN(lcAcctCode,'-',''))=0 .OR.;
                  (laData[5]<>"A".AND.lnTotdr=100.AND.lnTotcr=100),;
                  "DISABLE","ENABLE")
llNewDet    = IIF( VAL(STRTRAN(lcAcctCode,'-',''))=0 .OR.;
                  (laData[5]<>"A".AND.lnTotdr=100.AND.lnTotcr=100),;
                  .T.,.F.)
SHOW GET pbNew &lcObjState.

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
* VALID function for push button "New" (pbNew).
*
FUNCTION lfvNew

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

SELECT  (lc_TmpTrDt)

LOCATE FOR EMPTY(cAutType)
IF FOUND() 
  REPLACE cAutType WITH laData[1];
          cAutcode WITH laData[2];
          cStatus  WITH 'A'    
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpTrDt ;
             (cAutType,cAutcode,cStatus);
             VALUES (laData[1],laData[2],'A')
ENDIF             

*** Add Audit Information to the newly created record
=gfAdd_Info()

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
lcAcctcode  = REPLICATE("0",lnAcsSegSz) 
lcAccDesc   = SPACE(60)
lnDebit     = 0
lnCredit    = 0

*** Increase number of records in temporary file
lnTmpRcCnt  = lnTmpRcCnt + 1

*** Select the new record from the list
lsTrnDet    = lnTmpRcCnt

*** Refresh objects
SHOW GET lcAcctCode ENABLE
SHOW GET ibActBrow  ENABLE
SHOW GET lcAccDesc

*** Disable numeric fields until an account is entered.
SHOW GET lnDebit    DISABLE
SHOW GET lnCredit   DISABLE
SHOw GET pbAdj      DISABLE

IF lnTmpRcCnt = 1
  SHOW GET rbType   DISABLE
  SHOW GET pbRem    ENABLE
ENDIF

*** Disable New button until a valid account is ebtered
SHOW GET pbNew      DISABLE
llNewDet  = .T.

*** Do not forget to refresh the list
SHOW GET lsTrnDet  

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
_CUROBJ   = OBJNUM(lcAcctcode)

*** Always return to the original work aria
SELECT  GLAUTHD

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
  
  SELECT  (lc_TmpTrDt)
  
  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  lcStatus   = SUBSTR('DDS',AT(cStatus,'SMA'),1)   
  REPLACE cStatus WITH lcStatus
  
  *** Decrement number of records in list
  lnTmpRcCnt = lnTmpRcCnt - 1

  *** Adjust totals
  IF cDrOrCr = "D"
    lnTotDr  = lnTotDr - nAmount
  ELSE
    lnTotCr  = lnTotCr - nAmount
  ENDIF
  
  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF EOF(lc_TmpTrDt)
    GO TOP
  ENDIF

  lsTrnDet   = IIF(lnTmpRcCnt=0,1,IIF(lsTrnDet=lnTmpRcCnt+1,1,lsTrnDet))
  lnOldTmRec = lsTrnDet
  
  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  lcAcctCode = IIF(lnTmpRcCnt=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
  lcAccDesc  = IIF(lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                   &lc_TmpTrDt..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
 
  lnDebit    = IIF(lnTmpRcCnt=0 .OR. cDrOrCr="C",0,nAmount)
  lnCredit   = IIF(lnTmpRcCnt=0 .OR. cDrOrCr="D",0,nAmount)

  lcObjStat  = IIF(lnTmpRcCnt = 0,"DISABLE","ENABLE")
  SHOW GET lcAcctCode &lcObjStat.
  SHOW GET ibActBrow  &lcObjStat.
  SHOW GET lcAccDesc 

  SHOW GET lnDebit    &lcObjStat.
  SHOW GET lnCredit   &lcObjStat.

  SHOW GET pbRem      &lcObjStat.
  SHOW GET pbNew      ENABLE
  llNewDet  = .F.
  
  IF rbType = 1 .AND. lnTotDr <> lnTotCr
    lcObjStat = IIF(lnTmpRcCnt > 1 , "ENABLE","DISABLE")
  ELSE
    lcObjStat = "DISABLE"
  ENDIF
  SHOW GET pbAdj &lcObjStat.

  IF lnTmpRcCnt = 0
    SHOW GET rbType ENABLE
  ENDIF

  *** Update list contents
  SHOW GET lsTrnDet
  =lfRefresh()

  _CUROBJ  = OBJNUM(lsTrnDet)

  SELECT  GLAUTHD
   
ENDIF

*!**************************************************************************
*!
*!      Function: lfvAdj
*!
*!**************************************************************************
* VALID function for push button "Adjust" (pbAdj).
*  
FUNCTION lfvAdj

SELECT (lc_TmpTrDt)

lnTotDr  = lnTotDr - lnDebit 
lnTotCr  = lnTotCr - lnCredit

lnDiff   = lnTotdr - lnTotcr
lnSign   = SIGN(lnDiff)
lnDiff   = ABS (lnDiff)
*** If the difference is positive,i.e. Debit is greater,
IF lnSign = 1
  *** Add difference to Credit field
  lnTotcr   = lnTotCr + lnDiff
  lnCredit  = lnDiff
  lnDebit   = 0 
ELSE
  *** If the difference is negative,i.e.Credit is greater,
  IF lnSign  = -1
    lnTotdr  = lnTotdr + lnDiff 
    lnDebit  = lnDiff
    lnCredit = 0
  *** If the difference is zero,
  ELSE
    lnDebit  = 0
    lnCredit = 0
  ENDIF  
ENDIF

*** If both debit and credit fields are equal to 0, record the entry 
*** 'D'ebit (default)
REPLACE  cDrOrCr WITH IIF(lnCredit>0,"C","D"),;
         nAmount WITH lnDiff, ;
         cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)    

SHOW GET lsTrnDet
SHOW GET lnDebit
SHOW GET lnCredit
SHOW GET pbAdj DISABLE

=lfRefresh()
_CUROBJ = OBJNUM(pbSav)

SELECT GLAUTHD

IF VAL(STRTRAN(lcAcctCode,'-','')) > 0
  SHOW GET pbNew ENABLE
  llNewDet  = .F.
ENDIF  

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*    This procedure handles saving,instead of the global procedure.
*    "Save" option corresponds to the ninth position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global saving procedure.
*      The flag llCSave is a global flag that is to have the value
*    of "FALSE" (.F.) if the record(s) is/are not to be saved.
*
PROCEDURE lpSavScr

DO CASE
  *** If the file is empty,i.e. there are no template transactions,
  *** "Cannot save an empty template" 
  CASE (lnTmpRcCnt = 0 )
    =gfModalGen("TRM02035B00000","DIALOG",lcTTmpTr)
 
    llCSave  = .F.
    _CUROBJ  = OBJNUM(pbNew)
    RETURN

  CASE VAL(STRTRAN(lcAcctCode,'-',''))=0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave  = .F.
    _CUROBJ  = OBJNUM(lcAcctCode)
    RETURN

  *** If Total debits and credits are not equal,(in Amount mode)
  *** "Template entries must balance"
  CASE rbType = 1 .AND. lnTotDr <> lnTotCr
    =gfModalGen("TRM02019B00000","Dialog",lcTTmplt)
    llCSave  = .F.
    _CUROBJ  = OBJNUM(lsTrnDet)
    RETURN

  *** If Total debits and credits are not equal to 100,(in Percent mode)
  *** "Both must equal 100 % "
  CASE rbType = 2 .AND. (lnTotDr <> 100.00 .OR. lnTotCr <> 100.00)
    =gfModalGen("TRM02018B00000","Dialog",lcTDrCrPrc)
     llCSave  = .F.
     _CUROBJ  = OBJNUM(lsTrnDet)
    RETURN

  OTHERWISE 
    
    *** Attempt to lock files
    *** If 'A'dding a new header, attempt to lock 'GLAUTHD', 'GLAUTDT'
    *** files.
    *** If 'E'diting a record, attempt to lock the current record in 
    *** GLAUTHD file (the edited record), and  'GLAUTDT' file.
    *** If adding a new record,append a blank one
    
    IF laScrMode[4]
      APPEND BLANK
      =gfObj_Lock(.T.)
    ENDIF

    *** Store laData values in the current record
    GATHER FROM laData FIELDS &lcScFields

    *** Now save the data in the temporary file using the following global
    *** function which performs correct translation from the temporary
    *** file lc_TmpTrDt,and the main file GLAUTDT
    =gfTmp2Mast('GLAUTDT',lc_TmpTrDt,'Saving template '+laData[2]+' ...')
  
    SELECT GLAUTHD
    IF laScrMode[4]
      =gfObj_Lock(.F.)
    ENDIF
ENDCASE

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*    This procedure handles deletion,instead of the global procedure.
*    "Delete" option corresponds to the seventh position in laDefProc array,
*    hence it should have previously been assigned the value .F.
*    to disable the global delete procedure.
*
PROCEDURE lpDelScr

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
  laScrMode    = ""
  laScrMode[1] = .T.
  RETURN
ENDIF  

lnTotRec       = RECCOUNT(lc_TmpTrDt)+1
lnThermRec     = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpTrDt is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK

REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
       FOR &lcAutDtEx. = laData[1]+laData[2];
         .AND. lfThermo(;
            'Deleting template '+laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

IF lnThermRec < lnTotRec
  =gfThermo(lnTotRec,lnTotRec,'Deleting template '+laData[2]+' ...','')
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

lnThermRec = lnThermRec + 1

