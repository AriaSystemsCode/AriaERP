*:************************************************************************
*:
*: Procedure file: GLRCRTR.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: Renee Ezzat Ashamalla
*:      Copyright (c) 
*:  Last modified: 04/10/1994
*:
*:  Procs & Fncts
*:    PROCEDURE lpShow       *** local READ SHOW clause                 ***
*:    PROCEDURE lpSavScr     *** local <  Save  > procedure             ***
*:    PROCEDURE lpDelScr     *** local < Delete > procedure             ***
*:    FUNCTION  lfTotals     *** called from Select_SQL command         ***
*:    FUNCTION  lfvData_2    *** Valid function of laData[2]  get field ***
*:    FUNCTION  lfvData_5    *** Valid function of laData[5]  get field ***
*:    FUNCTION  lfvData_6    *** Valid function of laData[6]  get field ***
*:    FUNCTION  lfvData_7    *** Valid function of laData[7]  get field ***
*:    FUNCTION  lfvData_8    *** Valid function of laData[8]  get field ***
*:    FUNCTION  lfvSrcJrnl   *** Valid function of popup pusrcjrnl      ***
*:                           *** (WINDOWS), or ibSrcJrnl (DOS)          ***
*:    FUNCTION  lfvReverse   *** Valid function of check box cbReverse  ***
*:    FUNCTION  lfvDur       *** Valid function of popup puDuration     ***
*:                           *** (WINDOWS), or ibDuration (DOS)         ***
*:    FUNCTION  lswRcrDet    *** When  function of list lsRcrDet        ***
*:    FUNCTION  lfvAccCode   *** Valid function of cAcctCode  get field ***
*:    FUNCTION  lfvDebit     *** Valid function of lnDebit    get field ***
*:    FUNCTION  lfvredit     *** Valid function of lnCredit   get field ***
*:    FUNCTION  lfvNew       *** Valid function of push button pbNew    ***
*:    FUNCTION  lfvRem       *** Valid function of push button pbRem    ***
*:    FUNCTION  lfvAdj       *** Valid function of push button pbAdj    ***
*:    FUNCTION  lfSrcJrnl    *** Source journal array preparation for   ***
*:                           *** popup pusrcjrnl(WINDOWS), or ibSrcJrnl ***
*:                           *** (DOS)                                  ***
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*B600380,1 Reham On 06/05/95
*B600380,1 1- Move setting the defaults from the SCX setup to prevent the
*B600380,1    changing of the data if leaving the screen & back again 
*B600380,1    specially (S.J. popup - Duration popup - Reverse check box).
*B600380,1 2- Control the disabling & enabling of the <New> button in the
*B600380,1    different modes.

*B600385,1 Reham On 06/07/95
*B600385,1 In the key field, If coming from the browse to the select mode
*B600385,1 point to the current object which is the key field.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*B602247,1 KAM 04/09/2000 we add lines of code in screen which called;
*b602247,1 KAM            glrcrdt.scx in WHEN of account code object
*:************************************************************************
*
EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode,laTDur

DECLARE laKeyField [2,4],laTDur[2,1],laTSrcJrnl[1,2]
DECLARE laAutDtFld [1,4]       && holds the file structure of 'GLAUTDT' file
DIMENSION laWndObj[3,3]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'TYPECODE'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'TYPECODE'
laKeyField[2,4] = 2

laDefProc[7]    = .F.        && Use local Delete procedure 
laDefProc[9]    = .F.        && Use local Save   procedure 

*** Variables in screen object file :
*B600380,1 Reham On 06/05/95 Move the array from SCX to the prg to 
*B600380,1 set the right default data.
laTDur[1,1] = "Periods"
laTDur[2,1] = "Days"

lcTDfRef   = ""       && 'On'
lcTDfDesc  = ""       && 'Created by'
lcTFirst   = ""       && 'first'
lcTNext    = ""       && 'next'
lcTRcrTr   = ""       && 'recurring transaction|recurring transaction'
lcTDrOrCr  = ""       && 'either debit or credit'   
lcTRcr     = ""       && 'Recurring'

*** Screen display variables :
lcTTotal   = ""        && 'Totals :'
lcTDebit   = ""        && 'Debit'
lcTCredit  = ""        && 'Credit'

*** Begin date of previous fiscal year 
ldPreFBgDat = GOMONTH(ldFisBgDat,-12)   

*** Variables declaration 
lsRcrDet    = 1          && List variable ( pointer )
puSrcJrnl   = 1
puDuration  = 1          && "Duration " popup variable ,
                                && used in WINDOWS version only.  
cbReverse   = .F.        && Check box 'Reverse' (initially
                                && unchecked)

lcData5St  = 'DISABLE'
lcData6St  = 'DISABLE'

lcAcctCode = REPLICATE ("0",lnAcsSegSz)  
lcOldAcct  = ''         && Old Account number 
lcAccDesc  = ''         && corresponds to field GLAUTDT.cAccnlDes
lcCodeType = ''         && variable to hold the current account type 
lcRStamp   = ''         && Audit information of current record
lcFieldStr = ''         && list field string
lc_TmpRcr  = ''         && temporary file name variable
LCTRCRDT   = "Recurring details "
*lcTRcrDt   = ''         && Child window title
lcDuration = ''         && holds duration popup selection.
lcSrcJrnl  = ''         && holds the selection of the S.J popup
lcDSrcJrn  = ''         && default source journal string 
lcAutDtTg  = ''         && Tag name of GLAUTDT upon entry.      
lcAutDtEx  = ''         && Tag expression of GLAUTDT current tag 
lnTmpRcCnt = 0          && number of records in th list
lnOldRcRec = 0          && previously selected record (position) 
lnPopLen   = 0          && Source journal popup length
lnOldDr    = 0          && old debit value
lnOldCr    = 0          && old credit value
lnOldData8 = 0          && old laData[8]
lnDebit    = 0          && variable to accept debit  value
lnCredit   = 0          && variable to accept credit value
lnTotDr    = 0          && total debits  
lnTotCr    = 0          && total credits
lnSrcJrnl  = 0  
lnDSrcJrn  = 0          && Default source journal array index
lnAutDtFld = 0          && number of fields of 'GLAUTDT' file
lnThermRec = 0          && Thermometer counter
ldOldFDate = {}         && old value of first date
ldOldNDate = {}         && old value of next  date        
ldOldLDate = {}         && old value of last  date

llFromBton = .F.
llBrowse   = .F.

*B600380,1 Reham On 06/05/95 Set this flag to know that there is a detail
*B600380,1 line added without an account.
llNewDet   = .F.

*** Variables hold the status of the objects. ***
lcAct_Stat = ""
lcDeb_Stat = ""
lcCrd_Stat = ""
lcNew_Stat = ""
lcRem_Stat = ""
lcAdj_Stat = ""

IF !gfSetup()    
  RETURN
ENDIF  
lcCurrSign = '$'        && currency sign
lcUserName = gcUserName
*lcUserName = LOOKUP(SYUUSER.cUsr_Name,gcUser_ID,SYUUSER.cUser_ID,'cUser_ID')
lcCurDate  = IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                 LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))
lcRefFunc  = IIF(_DOS,'lfDOSRef','lfWinRef')

laWndObj[1,1] = gcBaseWind
laWndObj[1,2] = "IBBROWKEY"
laWndObj[1,3] = "PBENTRY"

laWndObj[2,1] = "GWCCONTRL1"
laWndObj[2,2] = "PBTOP"
laWndObj[2,3] = "PBCLS"

laWndObj[3,1] = "CWDGLRCRDT"
laWndObj[3,2] = "LSRCRDET"
laWndObj[3,3] = "PBDTCLOSE"

SELECT GLTYPES
GO TOP
IF EOF()
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

IF !WEXIST(gcBaseWind)
  SCATTER FIELDS &lcScFields TO laData BLANK
  laData[1]          = 'R'
  *** Create a name for the temporary file
  lc_TmpRcr        = gfTempName()

  SELECT GLAUTDT
  
  lcAutDtTg       = SYS(22)
  
  SET ORDER TO TAG TYPCODACC
  
  *** Current tag expression is cAutType + cAutCode + cAcctCode
  lcAutDtEx       = SYS(14,VAL(SYS(21)))

  *** Create the temporary file with the appropriate fields , 
  ***(in this case all fields of file "GLAUTDT" + 2 more fields
  *** for the record number and status('M'odify,'D'elete,'A'dd,'S'ame)
 
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

  CREATE TABLE &gcWorkDir.&lc_TmpRcr;
      FROM ARRAY laAutDtFld

  *** Prepare lcFieldStr for the list 
  SELECT (lc_TmpRcr)      

  DO CASE
    CASE _DOS
      lcFieldStr = "SUBSTR(&lc_TmpRcr..cAcctcode,1,lnAcsSegSz)+' '+"+;
                   "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpRcr..cAcctcode,"+;
                   "GLACCHAR.cAcctcode,'ACCTCODE'),1,40-lnAcsSegSz)+' '+"+;
                   "IIF(&lc_TmpRcr..cDrOrCr='D',"+;
                   "STR(&lc_TmpRcr..nAmount,15,2)+SPACE(12)+'0.00',"+;
                   "SPACE(11)+'0.00 '+STR(&lc_TmpRcr..nAmount,15,2))" 
    CASE _WINDOWS               
      lcFieldStr = "SUBSTR(&lc_TmpRcr..cAcctcode,1,lnAcsSegSz)+' '+"+;
                   "SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpRcr..cAcctcode,"+;
                   "GLACCHAR.cAcctcode,'ACCTCODE'),1,31-lnAcsSegSz)+' '+"+;
                   "IIF(&lc_TmpRcr..cDrOrCr='D',"+;
                   "STR(&lc_TmpRcr..nAmount,15,2)+SPACE(12)+'0.00',"+;
                   "SPACE(11)+'0.00 '+STR(&lc_TmpRcr..nAmount,15,2))" 
  ENDCASE

  *** Create source journal array for the source journals popup
  =lfSrcJrnl()
  
  *B600380,1 Reham On 06/05/95
  *B600380,1 This code was existed in the SCX & it was moved to prevent
  *B600380,1 putting wrong data if leaving the screen & back again.
  puDuration = 1
  lcDuration = laTDur[1,1]
  laData[9]  = "P"
  laData[11] = "N"
  
  puSrcJrnl  = lnDSrcJrn
  lcSrcJrnl  = lcDSrcJrn
  laData[12] = lcSJ_Def
ENDIF

SELECT GLAUTHD
SET FILTER TO
*** Filter on recurring transactions records only
SET FILTER TO CAUTTYPE = "R"
*B600380,1 Reham On 06/05/95 
*B600380,1 Change the command from "LOCATE" to the next command to prevent
*B600380,1 changing the record no. if leaving the screen to another one &
*B600380,1 back again in the GLAUTHD
LOCATE FOR CAUTCODE = laData[2]

*B600380,1 Reham On 06/05/95 Restore the right value if back to screen again.
cbReverse = IIF(laData[11] = 'Y' , .T. , .F.)

IF laScrMode[1] .OR. laScrMode[2]
  STORE "DISABLE" TO lcAct_Stat,lcDeb_Stat,lcCrd_Stat,;
                     lcNew_Stat,lcRem_Stat,lcAdj_Stat
ELSE
  lcAct_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  lcDeb_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  lcCrd_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  
  *B600380,1 Reham On 06/05/95
  *B600380,1 Control the disabling & enabling of the <NEW> button in the
  *B600380,1 different modes.
  lcNew_Stat = IIF(lnTmpRcCnt > 0 , ;
                   IIF(llNewDet , "DISABLE" , "ENABLE") , "ENABLE")
  
  lcRem_Stat = IIF(lnTmpRcCnt = 0 , "DISABLE" , "ENABLE")
  lcAdj_Stat = IIF(lnTotDr <> lnTotCr .AND. lnTmpRcCnt > 1 ,;
                  "ENABLE","DISABLE")
ENDIF

*E300683,5 Call *.SPR from screens directory
* DO GLRcrTr.SPR 
DO (gcScrDir + gcWinAppl + '\GLRcrTr.SPR')
*E300683,5 end   
 
SET FILTER TO 

IF glQuitting
  IF WEXIST('CWDGLRCRDT')
    RELEASE WINDOW CWDGLRCRDT
  ENDIF

  *** reset tag of GLAUTDT
  SELECT GLAUTDT
  IF !EMPTY(lcAutDtTg)
    SET ORDER TO TAG (lcAutDtTg)
  ELSE
    SET ORDER TO
  ENDIF

  *** Close open temporary files.  
  IF USED(lc_TmpRcr)
    USE IN ALIAS(lc_TmpRcr)
  ENDIF
  ERASE &gcWorkDir.&lc_TmpRcr..dbf  
ENDIF 

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

laData[1]          = 'R'
DO CASE

  *** "Select" mode (laScrMode[1]=.T.)
  CASE laScrMode[1]
    SELECT (lc_TmpRcr)
    lsRcrDet  = 1
    SHOW GET lsRcrDet  ENABLE
    
    *** Delete old data ( if any )
    ZAP
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsRcrDet    
    ON ERROR &lcErrSett.
   
    *** Initialize variables for display
    lnTmpRcCnt = 0    && No nonempty records in the temporary file
    lnOldRcRec = 1    
    
    *** Reset defaults
    laData[9]  = "P"
    laData[11] = "N"
    laData[12] = lcSJ_Def
    
    cbReverse  = .F.
    puDuration = 1
    puSrcJrnl  = lnDSrcJrn
    
    lcDuration = laTDur[1,1]         && 'Periods'
    lcSrcJrnl  = lcDSrcJrn
    
    lcAcctCode = REPLICATE ("0",lnAcsSegSz)  
    lcAccDesc  = SPACE(60)
    lcOldAcct  = ''
        
    lnCredit   = 0   
    lnDebit    = 0
    lnTotDr    = 0
    lnTotCr    = 0
    lnOldDr    = 0
    lnOldCr    = 0

    lcData5St  = 'DISABLE'
    lcData6St  = 'DISABLE'
    SHOW GET laData[5] &lcData5St
    SHOW GET laData[6] &lcData6St
    
    SHOW GET lcAcctCode  DISABLE
    SHOW GET ibActBrow   DISABLE 
    SHOW GET lnDebit     DISABLE 
    SHOW GET lnCredit    DISABLE 
    
    *** Controls
    SHOW GET pbNew       DISABLE 
    SHOW GET pbRem       DISABLE 
    SHOW GET pbAdj       DISABLE
    SHOW GET pbEntry     DISABLE 
    
  *** "View" mode (laScrMode[2]=.T.) ,or "Edit" mode (laScrMode[3]=.T.)
  CASE laScrMode[2].OR.laScrMode[3] 
    lnDebit    = 0
    lnCredit   = 0
    llFirstRec = .T.
    lnSrcJrnl  = IIF(lnPopLen>0,ASCAN('laTSrcJrnl',laData[12]),0)
      
    *** Header file object
    DO CASE
      CASE _DOS
        lcDuration = IIF(laData[9] $ 'PD',;
                     laTDur[AT(laData[9],'PD'),1],'')
        lcSrcJrnl  = IIF(lnSrcJrnl>0,;
                     laTSrcJrnl[ASUBSCRIPT('laTSrcJrnl',lnSrcJrnl,1),1],;
                     lcDSrcJrn)    
      CASE _WINDOWS
        puDuration = AT(laData[9],'PD') 
        puSrcJrnl  = IIF(lnSrcJrnl>0,;
                      ASUBSCRIPT('laTSrcJrnl',lnSrcJrnl,1),lnDSrcJrn)
    ENDCASE
    
    lcRStamp       = IIF(laScrMode[2], GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time,lcRStamp)
        
    IF laScrMode[2] .OR. lcRStamp <> GLAUTHD.cAdd_User+DTOC(GLAUTHD.dAdd_Date);
                        +GLAUTHD.cAdd_Time

      *** The file is selected for lfTotals()   
      SELECT GLAUTDT  
      lnTotDr   = 0
      lnTotCr   = 0

      SELECT *,RECNO() AS 'nRecNo',	"S" AS 'cStatus';
             FROM &gcDataDir.GLAUTDT;
             INTO DBF &gcWorkDir.&lc_TmpRcr;
             WHERE &lcAutDtEx. = "R"+laData[2];
             .AND. lfTotals()
    ENDIF         
           
    SELECT (lc_TmpRcr)

    *** Get the type of accounts
    IF laScrMode[3]
      lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpRcr..cAcctCode,;
                       GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
    ENDIF                             

    *** Get the number of records currently in the temporary file
    lnTmpRcCnt = RECCOUNT(lc_TmpRcr)

   *** Select the previously selected record,( lnOldRcRec)
   lsRcrDet   = IIF(laScrMode[2],1,lnOldRcRec)
   lnOldRcRec = lsRcrDet
      
    IF RECCOUNT(lc_TmpRcr) > 0
      GO lnOldRcRec
    ENDIF   

    *** Considering the fact that there has to be at least one record
    *** in the temporary file,( due to the fact that we cannot "Save"
    *** an empty recurring trasnsaction,there 
    *** seems to be no reason for checking for the number of
    *** records in the file,or to initialize variables to spaces,hence,
    
    lcAcctCode = cAcctCode
    lcAccDesc  = LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpRcr..cAcctCode,;
                        GLACCHAR.cAcctCode,'ACCTCODE')
    IF cDrorCr="D"
      lnDebit   = nAmount
    ELSE
      lnCredit  = nAmount
    ENDIF
    
    cbReverse = IIF(laData[11] = 'Y' , .T. , .F.)
    
    *** Prepare screen objects
    lcObjState = IIF(laScrMode[3],"ENABLE","DISABLE")
    SHOW GET pbRem      &lcObjState.
    SHOW GET pbNew      &lcObjState.
    
    SHOW GET pbEntry    ENABLE
    
    SHOW GET lcAcctCode &lcObjState.
    SHOW GET ibActBrow  &lcObjState.
    SHOW GET lnDebit    &lcObjState.
    SHOW GET lnCredit   &lcObjState.

    *** If returning to 'View' mode from 'Edit' mode with the Adjust
    *** button enabled,we'd like to disable it.
    IF laScrMode[2]
      SHOW GET pbAdj   DISABLE    
    ENDIF   
    
    *** Disable error handler until the list is refreshed,then
    *** Enable it again
    lcErrSett      = ON("ERROR")
    ON ERROR lnDum = 1
    SHOW GET lsRcrDet     ENABLE
    ON ERROR &lcErrSett.

    IF laScrMode[2]
      lcData5St  = 'DISABLE'
      lcData6St  = 'DISABLE'
    ELSE
      lcData5St  = 'DISABLE'
      lcData6St  = 'ENABLE'
    ENDIF

    SHOW GET laData[5] &lcData5St
    SHOW GET laData[6] &lcData6St

  *** "Add" mode (laScrMode[4]=.T.) 
  CASE laScrMode[4] 
    *** Prepare defaults :
    *** Remember that lcCurDate's length depends upon the century setting.

    lcData5St  = 'ENABLE'
    lcData6St  = 'DISABLE'
    SHOW GET laData[5] &lcData5St
    SHOW GET laData[6] &lcData6St

    lcDfRef    = lcTDfRef+' '+lcCurDate
    laData[3]  = lcDfRef+SPACE(FSIZE('cAutRef','GLAUTHD')-LEN(lcDfRef))  
    laData[4]  = SUBSTR(lcTDfDesc+' '+lcUsername,1,;     
                        FSIZE('cAutDes','GLAUTHD'))
    laData[5]  = ldFisBgDat 
    laData[6]  = ldFisBgDat 
    laData[7]  = ldFisEnDat
    laData[8]  = 1 

    SHOW GET pbRem       DISABLE
    SHOW GET pbAdj       DISABLE
    SHOW GET pbNew       ENABLE
    SHOW GET pbEntry     ENABLE
    SHOW GET lsRcrDet   
ENDCASE

SELECT GLAUTHD 

*!**************************************************************************
*!
*!      Function : lfTotals
*!
*!**************************************************************************
* This function gets the total debits and credits,called
* while SELECT_SQL command is executed.
*
FUNCTION lfTotals

*** Skip first record (SELECT SQL counts it twice),first time
IF llFirstRec
  llFirstRec = .F.
ELSE
  IF cDrOrCr ="D"
    lnTotdr   = lnTotdr  + nAmount
  ELSE
    lnTotcr   = lnTotcr  + nAmount
  ENDIF
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
laData[1]  = 'R'

*!**************************************************************************
*!
*!      Function: lfvSrcJrnl
*!
*!**************************************************************************
*
FUNCTION lfvSrcJrnl

=lfSrcJrnl()

DO CASE
  CASE _DOS
    laData[12] = gfActPop(4,21,lnPopLen+6,67,'laTSrcJrnl',2,1,@lcSrcJrnl)
    =lfRefresh()  
  CASE _WINDOWS
    laData[12] = laTSrcJrnl[puSrcJrnl,2]
ENDCASE    

*!**************************************************************************
*!
*!      Function: lfvData_5
*!
*!**************************************************************************
*    VALID function for the get field "laData[5]"
*    It corresponds to the first transaction date 
*    This field is editable only in ADD mode ( laScrMode[4] )
FUNCTION lfvData_5

*** If the first date is less than the start date of the previous
*** fiscal year,do not accept the entry,else use the same entry
*** for the next transaction date, which is disabled in this mode.
IF laData[5] < ldPreFBgDat 
  =gfModalGen("TRM02057B00000","DIALOG",lcTFirst)  
  laData[5] = ldOldFDate 
  SHOW GET laData[5]
  _CUROBJ=OBJNUM(laData[5])
ELSE
  *** If first date is greater than last transacrion date,do not accept it
  IF laData[5] > laData[7]
    =gfModalGen("TRM02058B00000","DIALOG",lcTFirst)  
    laData[5] = ldOldFDate 
    SHOW GET laData[5]
    _CUROBJ   = OBJNUM(laData[5])
  ELSE
    laData[6] = laData[5]
    SHOW GET laData[6]  
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_6
*!
*!**************************************************************************
*    VALID function for the get field "laData[6]" 
*    It corresponds to the next transaction date 
*    This field is editable only in EDIT mode ( laScrMode[3] )

FUNCTION lfvData_6

*** If the next date is less than the start date of the previous
*** fiscal year,do not accept the entry,else use the same entry
*** for the next transaction date, which is disabled in this mode.
IF laData[6] < ldPreFBgDat
  =gfModalGen("TRM02057B00000","DIALOG",lcTNext)  
  laData[6]  = ldOldNDate 
  SHOW GET laData[6]
  _CUROBJ    = OBJNUM(laData[6])
ELSE
  *** If next date is greater than last transacrion date,do not accept it
  IF laData[6] > laData[7]
    =gfModalGen("TRM02058B00000","DIALOG",lcTNext)  
    laData[6]  = ldOldNDate 
    SHOW GET laData[6]
    _CUROBJ    = OBJNUM(laData[6])
  ELSE
    *** If next transaction date is less than the first transaction date,
    *** Copy the new value to the first transaction date. 
    IF laData[6] < laData[5]
      laData[5] = laData[6]
      SHOW GET laData[5]  
    ENDIF  
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*    VALID function for the get field "laData[7]"
*    It corresponds to the last transaction date 
*
FUNCTION lfvData_7

*** If the last date is less than the next transaction date, 
*** do not accept the entry,
IF laData[7] < laData[6] 
  =gfModalGen("TRM02058B00000","DIALOG",IIF(laScrMode[4],lcTFirst,lcTNext))  
  laData[7] = ldOldLDate
  SHOW GET laData[7]
  _CUROBJ   = OBJNUM(laData[7])
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_8
*!
*!**************************************************************************
*    VALID function for the get field "laData[8]"
*    It corresponds to field nAutFrqNo
*
FUNCTION lfvData_8

*** Reject negative or zero entries
IF laData[8] <= 0
  =gfModalGen(IIF(laData[8]<0,"TRM02036B00000","TRM02037B00000"),"DIALOG")
  laData[8] = lnOldData8
  SHOW GET laData[8]
  _CUROBJ   = OBJNUM(laData[8])
ENDIF

*!**************************************************************************
*!
*!      Function: lfvReverse
*!
*!**************************************************************************
*    VALID function for the check box 'Reverse ( cbReverse )
*    It corresponds to field cAutRev ( laData[11] ) 
*
FUNCTION lfvReverse

laData[11] = IIF(cbReverse,'Y','N')

*!**************************************************************************
*!
*!      Function: lfvDur
*!
*!**************************************************************************
*
FUNCTION lfvDur

DO CASE
  CASE _DOS
    laData[9] = SUBSTR('PD',ASCAN(laTDur,;
                       gfActPop(12,28,16,38,'laTDur',1,1,@lcDuration)),1)
    =lfRefresh()  
  CASE _WINDOWS
    laData[9] = SUBSTR('PD',puDuration,1)
ENDCASE    

*!**************************************************************************
*!
*!      Function: lswRcrDet
*!
*!**************************************************************************
*    WHEN function for the list "lsRcrDet" 
*
FUNCTION lswRcrDet

*** IF There are no records in the list,
*** prohibit selection from list.
IF  lnTmpRcCnt = 0 
  _CUROBJ = OBJNUM(pbNew) 
  RETURN .F.
ENDIF  

*** If current record has no account code entry,
*** prohibit selection from list.
IF VAL(STRTRAN(lcAcctCode,'-','')) = 0 .OR. ;
      (lnDebit = 0 .AND. lnCredit = 0)
  RETURN .F.
ENDIF

SELECT (lc_TmpRcr)

*** Refresh get fields with current contents
lcAcctCode = IIF(VAL(cAcctCode)=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
lcAccDesc  = IIF(VAL(cAcctCode)=0,SPACE(60),;
                 LOOKUP(GLACCHAR.cAccnlDes,&lc_TmpRcr..cAcctCode,;
                 GLACCHAR.cAcctCode,'ACCTCODE'))
IF cDrOrCr = "D"
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
lnOldRcRec = lsRcrDet

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
    _CUROBJ = OBJNUM(lcacctcode)
  CASE LASTKEY() = 15 .OR. LASTKEY() = 5
    _CUROBJ = OBJNUM(lsRcrDet)
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

IF LEFT(LTRIM(lcAcctCode),1) <> '?'.AND. ;
   !ISDIGIT(LTRIM(lcAcctCode))
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
      lcCodeType = "A"
    ENDIF
    IF lfVldAccnt(lcCodeType,"C","L",.T.,@lcAccDesc,@lcCodeT,"")
      IF lcCodeType = "A"
        lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
      ENDIF    

      SELECT (lc_TmpRcr)

      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       
      lcStatus = SUBSTR("MAM",AT(cStatus,"MAS"),1)

      REPLACE cAcctcode WITH lcAcctCode ,;
              cStatus   WITH lcStatus

      *** Refresh objects
      SHOW GET lsRcrDet
      SHOW GET lcAccDesc
      =lfRefresh()  

      SELECT GLAUTHD
  
      *** Enable "New" push button (pbNew) only if either a debit or a credit
      *** value is entered because in this screen, an entry is not accepted
      *** unless either of the debit or credit fields has a value greater than zero.
      IF lnDebit > 0 .OR. lnCredit > 0
        *B600380,1 Reham On 06/05/95
        *B600380,1 This flag added to know that the account is added
        *B600380,1 for the new detail line.
        llNewDet   = .F.
        SHOW GET pbNew ENABLE
        lcNextObj  = IIF(lnTotCr=lnTotDr,"pbSav","pbNew")
        _CUROBJ    = OBJNUM(&lcNextObj.)    
      ELSE
        SHOW GET lnDebit  ENABLE
        SHOW GET lnCredit ENABLE
        _CUROBJ    = OBJNUM(lnDebit)  
      ENDIF  

      lcObjState   = IIF(lnTotDr<>lnTotCr .AND. lnTmpRcCnt > 1,;
                         "ENABLE","DISABLE")
      SHOW GET pbAdj &lcObjState
    ELSE
      lcAcctCode   = lcOldAcct
      SHOW GET lcAcctCode
      _CUROBJ      = OBJNUM(lcAcctCode)
    ENDIF
    CLEAR TYPEAHEAD
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    lnTotDr field is used for totals
*   
FUNCTION lfvDebit

IF lnDebit <> lnOldDr
    
  *** Reject negative entries
  IF lnDebit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnDebit   = lnOldDr  
    SHOW GET lnDebit
    _CUROBJ   = OBJNUM(lnDebit)
  ELSE 

    SELECT (lc_TmpRcr)

    *** If the record has been previously saved as a credit,
    *** and now there is a debit entry,
    IF lnDebit > 0 .AND. cDrOrCr = "C"

      *** Ignore the credit entry,blank it and adjust Credit totals. 
      lnCredit  = 0
      lnTotCr   = lnTotCr - nAmount  
      lnTotDr   = lnTotDr + lnDebit
       
      SHOW GET lnCredit    

    ELSE 
      lnTotDr   = lnTotDr-nAmount+lnDebit
    ENDIF
    lcStatus    = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnDebit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "D" 

    SELECT GLAUTHD
    
    *** Adjust controls
    SHOW GET lsRcrDet
    lcObjState = IIF(lnTotDr <> lnTotCr .AND. lnTmpRcCnt > 1 ,;
                     "ENABLE","DISABLE")
    SHOW GET pbAdj     &lcObjState
    =lfRefresh()
   
    lcNextObj  = IIF(lnTotDr=lnTotCr,"pbSav","pbNew")
    _CUROBJ    = OBJNUM(&lcNextObj)
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
                .AND.(lnDebit>0 .OR. lnCredit>0),"ENABLE","DISABLE")
*B600380,1 Reham On 06/05/95
*B600380,1 This flag to know the status of the <New> button.
llNewDet = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
                .AND.(lnDebit>0 .OR. lnCredit>0),.F.,.T.)
SHOW GET pbNew &lcObjState

*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
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
    lnCredit  = lnOldCr  
    SHOW GET lnCredit
    _CUROBJ   = OBJNUM(lnCredit)
  ELSE 
    SELECT (lc_TmpRcr)
    
    IF lnCredit > 0 .AND. cDrOrCr = "D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals. 
        lnDebit   = 0
        lnTotDr   = lnTotDr - nAmount  
        lnTotCr   = lnTotCr + lnCredit

        SHOW GET lnDebit
    ELSE
        lnTotCr = lnTotCr - nAmount + lnCredit
    ENDIF
    lcStatus    = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnCredit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "C" 
    
    *** Adjust controls
    SHOW GET lsRcrDet
    lcObjState = IIF(lnTotDr <> lnTotCr .AND. lnTmpRcCnt > 1,;
                     "ENABLE","DISABLE")
    SHOW GET pbAdj &lcObjState.
    =lfRefresh() 

    IF lnTotdr = lnTotcr 
      _CUROBJ = OBJNUM(pbSav)
    ENDIF
    
    SELECT GLAUTHD
  ENDIF
ENDIF

lcObjState = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
                 .AND.(lnDebit>0 .OR. lnCredit>0),"ENABLE","DISABLE")
*B600380,1 Reham On 06/05/95
*B600380,1 This flag to know the status of the <New> button.
llNewDet = IIF(VAL(STRTRAN(lcAcctCode,'-','')) > 0 ;
               .AND.(lnDebit>0 .OR. lnCredit>0),.F.,.T.)
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
*** else Insert a new record and prepare it to be filled  the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

SELECT (lc_TmpRcr)

LOCATE FOR EMPTY(cAutType)
IF FOUND()
  REPLACE cAutType WITH laData[1],;
          cAutCode WITH laData[2],;   
          cStatus  WITH 'A'
ELSE
  INSERT INTO &gcWorkDir.&lc_TmpRcr.;
              (cAutType,cAutCode,cStatus);
         VALUES (laData[1],laData[2],"A")      
ENDIF

*** Add Audit Information to the newly created record
=gfAdd_Info()

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 
lcAcctCode = REPLICATE ("0",lnAcsSegSz)  
lcAccDesc  = SPACE(60)

lnDebit    = 0
lnCredit   = 0

*** Increase number of records in temporary file
lnTmpRcCnt = lnTmpRcCnt + 1

*** Select the new record from the list
lsRcrDet   = lnTmpRcCnt

SHOW GET lcAcctCode ENABLE
SHOW GET ibActBrow  ENABLE
SHOW GET lcAccDesc

*** Disable numeric fields until an account is entered.
SHOW GET lnDebit    DISABLE
SHOW GET lnCredit   DISABLE
SHOw GET pbAdj      DISABLE

IF lnTmpRcCnt > 0
  SHOW GET pbRem    ENABLE
ENDIF

*** Disable New button until a valid account is ebtered
SHOW GET pbNew      DISABLE

*B600380,1 Reham On 06/05/95
*B600380,1 This flag is added to know that there is a new detail line
*B600380,1 added with an empty account.
llNewDet = .T.

*** Do not forget to refresh the list
SHOW GET lsRcrDet     

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
 
  SELECT (lc_TmpRcr)

  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  REPLACE cStatus WITH SUBSTR("DSD",AT(cStatus,"MAS"),1)

  *** Decrement number of records in list
  lnTmpRcCnt = lnTmpRcCnt - 1
  
  *** Adjust totals
  IF cDrOrCr = "D" 
    lnTotDr = lnTotDr - nAmount
  ELSE 
    lnTotCr = lnTotCr - nAmount
  ENDIF
  
  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF EOF(lc_TmpRcr)
    GO TOP
  ENDIF  
 
  lsRcrDet   = IIF(lnTmpRcCnt=0,1,IIF(lsRcrDet=lnTmpRcCnt+1,1,lsRcrDet))
  lnOldRcRec = lsRcrDet

  *** Refresh objects with contents of the current record,or spaces
  *** if the list is empty
  lcAcctCode = IIF(lnTmpRcCnt=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
  lcAccDesc  = IIF(lnTmpRcCnt=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                   &lc_TmpRcr..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
 
  lnDebit    = IIF(lnTmpRcCnt=0 .OR. cDrOrCr="C",0,nAmount)
  lnCredit   = IIF(lnTmpRcCnt=0 .OR. cDrOrCr="D",0,nAmount)
  
  *** Adjust controls
  lcObjState = IIF(lnTmpRcCnt=0,"DISABLE","ENABLE")

  SHOW GET lcAcctCode &lcObjState.       
  SHOW GET ibActBrow  &lcObjState.    
  SHOW GET lcAccDesc 

  SHOW GET lnDebit    &lcObjState.
  SHOW GET lnCredit   &lcObjState.

  SHOW GET pbRem      &lcObjState.
  
  *B600380,1 Reham On 06/05/95
  *B600380,1 This is to know that I removed the added detail line.
  llNewDet = .F.
  SHOW GET pbNew      ENABLE
 
  lcObjState = IIF(lnTotDr <> lnTotCr .AND. lnTmpRcCnt > 1,;
                   "ENABLE","DISABLE") 
  SHOW GET pbAdj      &lcObjState.

  *** Update list contents
  SHOW GET lsRcrDet
  =lfRefresh()

  _CUROBJ = OBJNUM(lsRcrDet)
  
  SELECT GLAUTHD

ENDIF

*!**************************************************************************
*!
*!      Function: lfvAdj
*!
*!**************************************************************************
*    VALID function for push button "Adjust" (pbAdj).
*  
FUNCTION lfvAdj

SELECT (lc_TmpRcr)

lnTotdr   = lnTotdr - lnDebit 
lnTotcr   = lnTotcr - lnCredit
lnDiff    = lnTotdr - lnTotcr
lnSign    = SIGN(lnDiff)
lnDiff    = ABS (lnDiff)

*** If the difference is not equal to zero
IF lnDiff <> 0
  *** If the difference is positive,i.e. Debit is greater,
  IF lnSign = 1
    *** Add difference to Credit field
    lnTotcr   = lnTotcr + lnDiff
    lnCredit  = lnDiff
    lnDebit   = 0 
  ELSE
    *** If the difference is negative,i.e.Credit is greater,
    lnTotdr   = lnTotdr + lnDiff 
    lnDebit   = lnDiff
    lnCredit  = 0
  ENDIF

  REPLACE  cDrOrCr WITH IIF(lnDebit<>0,"D","C");
           nAmount WITH lnDiff ;
           cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)    

  SHOW GET lsRcrDet
  SHOW GET lnDebit
  SHOW GET lnCredit
  SHOW GET pbAdj DISABLE
  _CUROBJ = OBJNUM(pbSav)

  *** If the difference is zero,
ELSE
  *** Message :  "  By adjusting this entry, both debit     "
  ***            "  and credit will have zero values.       "
  ***            "  You cannot leave both debit and credit  "
  ***            "  with zero value.                        "
  ***                          <   OK >
  =gfModalGen("TRM02059B00000","DIALOG")
  lnTotdr   = lnTotdr + lnDebit 
  lnTotcr   = lnTotcr + lnCredit
ENDIF

=lfRefresh()

SELECT GLAUTHD

IF VAL(STRTRAN(lcAcctCode,'-',''))>0.AND.(lnDebit>0 .OR. lnCredit>0)
  *B600380,1 Reham On 06/05/95
  *B600380,1 This is to know that the <new> button status is enable.
  llNewDet = .F.
  SHOW GET pbNew ENABLE
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

  *** If the file is empty,i.e. there are no recurring transactions,
  *** "Cannot save an empty transaction" 
  CASE (lnTmpRcCnt = 0 )
    =gfModalGen("TRM02035B00000","Dialog",lcTRcrTr)
    llCSave = .F.
    _CUROBJ = OBJNUM(pbNew)
    RETURN
  CASE VAL(STRTRAN(lcAcctCode,'-','')) = 0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave = .F.
    _CUROBJ = OBJNUM(lcAcctCode)
    RETURN
  CASE lnDebit = 0 .AND. lnCredit = 0
    =gfModalGen("TRM02033B00000","DIALOG",lcTDrOrCr)
    llCSave = .F.
    _CUROBJ = OBJNUM(lnDebit)
    RETURN
  *** If Total debits and credits are not equal,
  *** "Recurring entries must balance"
  CASE ( lnTotcr <> lnTotdr )
    =gfModalGen("TRM02019B00000","Dialog",lcTRcr)
    llCSave = .F.
    _CUROBJ = OBJNUM(lsRcrDet)
    RETURN
  OTHERWISE 
    *** If adding a new record,append a blank one
    IF laScrMode[4]
      APPEND BLANK
      =gfObj_Lock(.T.)
    ENDIF

    *** Store laData values in the current record
    GATHER FROM laData fields &lcScFields.

    *** Now save the data in the temporary file using the following global
    *** function which performs correct translation from the temporary
    *** file lc_TmpRcr,and the main file GLAUTDT
    =gfTmp2Mast("GLAUTDT",lc_TmpRcr,;
                'Saving recurring entry '+laData[2]+' ...')  
 
    SELECT GLAUTHD
    IF laScrMode[4]
      =gfObj_Lock(.F.)
    ENDIF
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

lnTotRec       = RECCOUNT(lc_TmpRcr)+1
lnThermRec     = 0

*** Delete records belonging to the current header from the master
*** file (GLAUTDT)
*** The temporary file lc_TmpRcr is zapped in 'Select' mode
SELECT GLAUTDT
SCATTER MEMVAR MEMO BLANK

REPLACE cAutType  WITH m.cAutType,;
        cAutCode  WITH m.cAutCode,;
        cAcctCode WITH m.cAcctCode ;        
       FOR &lcAutDtEx. = laData[1]+laData[2];
         .AND. lfThermo(;
            'Deleting recurring entry '+laData[2]+' ...')
DELETE FOR &lcAutDtEx. = m.cAutType + m.cAutCode 

SELECT GLAUTHD

*** Then delete the header record
SCATTER MEMVAR MEMO BLANK
GATHER MEMVAR MEMO
DELETE 

IF lnThermRec < lnTotRec
  =gfThermo(lnTotRec,lnTotRec,;
            'Deleting recurring entry '+laData[2]+' ...','')
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

*!**************************************************************************
*!
*!      Function: lfSrcJrnl
*!
*!**************************************************************************
*  Creates an array of Source journals for the Source Journals popup
*
FUNCTION lfSrcJrnl

*** Create an array holding avialable Source Journals from 
*** GLSORJOR.DBF to be displayed in popup (puSrcJrnl/ibSrcJrnl)
SELECT ALLTRIM(CSRCJRNL)+'-'+;
       CJORLNDES,CSRCJRNL;
       FROM &gcDataDir.GLSUBJOR;
       INTO ARRAY laTSrcJrnl

*** Define a length for the DOS popup.
lnPopLen = IIF(_TALLY > 5 , 5 , _TALLY )
  
*** Get the row number of the array element corresponding to
*** the default source journal if the array is not empty.
lnDSrcJrn = IIF(lnPopLen>0, ASCAN('laTSrcJrnl',lcSJ_Def), 0)  
  
*** If the array is empty (_TALLY=0), add the default Source Journal
*** value (global variable lcSJ_Def) to the array
*** Else, look for the default Source journal value in the array, if
*** found, use it as a default in 'Select' mode and 'Add' mode, else
*** add the default source journal values (global variable lcSJ_Def) 
*** to the array.

IF lnDSrcJrn = 0
  lnDSrcJrn = IIF(lnPopLen = 0, 1, ALEN('laTSrcJrnl',1) + 1)
   
  DIMENSION laTSrcJrnl[lnDSrcJrn,2]
  laTSrcJrnl[lnDSrcJrn,1] = lcSJ_Def+'-'+''
*  laTSrcJrnl[lnDSrcJrn,1] = lcSJ_Def+'-'+lcSJ_Desc    
  laTSrcJrnl[lnDSrcJrn,2] = lcSJ_Def 
  lnPopLen  = lnPopLen + 1
ELSE
  lnDSrcJrn = ASUBSCRIPT('laTSrcJrnl',lnDSrcJrn,1)     
ENDIF 
  
*** Default source journal description is still missing in the 
*** last parameter of the following expression 
*** (lcSJ_Def+'-'+default source journal description) 
lcDSrcJrn = laTSrcJrnl[lnDSrcJrn,1]
