*:*************************************************************************
*:                                                                      ***
*: Procedure file: GLTRANS.PRG                                          ***
*:                                                                      ***
*:         System: ARIA BUSINESS SYSTEM                                 ***
*:         Module: General Ledger                                       ***
*:         Author: Hany Wadie                                           ***
*:      Copyright (c)                                                   ***
*:  Last modified:  24/03/1994                                          ***
*:                                                                      ***
*:  Procs & Fncts:                                                      ***
*:                                                                      ***
*:    PROCEDURE lpShow      *** Procdure to control screen modes        ***
*:    FUNCTION  lfvData_2   *** function to test valid key              *** 
*:    FUNCTION  lfvAddTrn   *** function to add new transaction         ***
*:    FUNCTION  lfvVldDate  *** function to test valid trans. date      ***
*:    FUNCTION  lfvData_16  *** function to test valid Reves. date      ***                                        ***
*:    FUNCTION  lfvPopup    *** function to obtain valid suorce journal ***
*:    FUNCTION  lfvHold     *** function to check valid Hold status     ***
*:    FUNCTION  lfvEntries  *** function to branch to detailes screen   ***
*:    FUNCTION  lfvPost     *** function to branch to post module       ***
*:    FUNCTION  lfvPrint    *** function to print reports               ***
*:    FUNCTION  lfwTrnDet   *** function to update fields from list     ***
*:    FUNCTION  lfvNew      *** function to add rew transaction line    ***
*:    FUNCTION  lfvRem      *** function to remove transaction line     ***
*:    FUNCTION  lfvAccCode  *** function to validate account code       ***
*:    FUNCTION  lfvDebit    *** function to validate debit amount       ***
*:    FUNCTION  lfvCredit   *** function to validate credit amount      ***
*:    FUNCTION  lfvTrdtexp  *** function to validation to decription    ***
*:    FUNCTION  lfvAdjust   *** function to adjust transaction balance  ***
*:    FUNCTION  lfTrnDet    *** function to prepare list string         ***
*:    PROCEDURE lpSavScr    *** procedure to save transaction           ***
*:    PROCEDURE lpDelScr    *** procedure to void transaction           ***
*:    FUNCTION  lfvTemp     *** function to add automatic template      ***
*:    FUNCTION  lfDist      *** function to add distribution entries    ***
*:    FUNCTION  lfStat      *** function to update hold & status        ***
*:    FUNCTION  lfControl   *** function to enable obj. in child window ***
*:    FUNCTION  lfCheckRec  *** function to check modi. by another user ***
*:                                                                      ***
*:                                                                      ***
*:      Documented 24/03/1994                                           ***
*:*************************************************************************
*B600390,1 Reham 06/08/95
*B600390,1 1- Put the same account status variable for the account browse 
*B600390,1    invisable button in the SCX
*B600390,1 2- Refresh the acc. browse invisable button with every account
*B600390,1    refreshing in the different modes.

*B600391,1 Reham On 06/08/95
*B600391,1 Refresh the account desc. & the comment if add new account.

*B600392,1 Reham On 06/08/95
*B600392,1 If coming from the view mode in the batch screen, in the 
*B600392,1 validation of the key field, allow only a message with 2
*B600392,1 buttons <Browse> - <Reenter> {exclude the <Add> button}
*B600392,1 to prevent going to the add mode & back again to the 
*B600392,1 select mode
*
*B500782,1 Reham On 06/12/95 Reresh the total debit & credit & balance 
*B500782,1 in the transction screen.
*
*B600486,1 Reham On 06/21/95
*B600486,1 Control the array elemets that hold the control pannel button
*B600486,1 status for <Delete> button "laCtrStat[8]" & for <Edit> button
*B600486,1 "laCtrStat[7]" to control the the edit & delete Bars in the
*B600486,1 record pad.
*B600499,1 ORABY 07/03/1995 Disable the Remove Entries button in the View mode
*B600513,1 ORABY 07/10/1995 If comming from the batches screen in the view mode 
*B600513,1                  we can not select a transaction except through the browse
*B600549,1 ORABY 07/10/1995 The template entries validation never finds the right record
*B600552,1 ORABY 07/11/1995 The Entries screen fields are enabled in the view mode
*B600930,1 Hesham El-Sheltawi 04/01/96
*B600930,1 check for the allowing single transactions in the glsetup file
*B600930,1 if ! allowed then display message and return
*B601099,1 Hide pbPrint from the screen by change the #OBJDISP from "!llFromBat" to ".F."
*B800691,1 M.H 07/30/96 Fix the bug for allow single transaction from batch if glsetup.allow single account = .F.
*B601449,1 HS  12/02/96 Fix the Comment field in the
*B601449,1              GLTRDT.SCX [the Entries screen] and the 
*B601449,1              Transaction No. Browse when coming from the Batch screen 
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*B601810,1 MAN 07/10/97. Fix list refreshing under Windows
*B801123,1 AMM 07/29/97. Refresh Totals in trans screen
*B801455,1 AMM 02/04/98 1-Fixed the bug of dosn't clear the reverse period and 
*B801455,1 AMM          year even when deleting the reverse date.
*B801455,1 AMM          2- Resize the description field to fit the size of the 
*B801455,1 AMM          field in windows version.
*E301077,71   Walid Abou El-Magd (WAM)
*          Open GLSETUP file with gfOpenFile()
*B602801,1 RENEE 04/15/99. 1. Repostion file pointed on the calling batch if
*B602801,1				      called 
*B602803,1 WALID 04/18/99 Fix the validation of transaction when calling
*          from journal batch and brows by mouse .
*B602311,1 KAM 09/11/2000 fix the situation when user press Esc key 
*B607000,1 WAB 03/03/2003  Fix the bug of Cance.bmp not found
*B607109,1 ALB 08/14/2003 Enable the copy&past function when the call from Jornal batch screen
*:***********************************************************************************************
*
*** Parameter lcBatchno used to pass batch number if the transaction screen
*** used from journal batches screen
*** lcVewMode used to keep currnent view mode if comming from same screen
*** lcEntOrTr used to check if transaction or entry is required

PARAMETERS pcBatchno,pcTrnHdRec,pcTrnDtRec,pcVewMode,pcEntOrTr
EXTERNAL ARRAY laData,laKeyField,laDefProc,laScrMode,laCtrStat
*** Check if comming from batch program or not
*** Array decleration 
DECLARE laKeyField [2,4]       && Array to hold key exprtion
DECLARE laTStatus  [3,1]       && Array to hold transaction status
DECLARE laSourcJor [1,2]       && Array to hold source journal
DECLARE laWndObj   [3,3]       && Have screen name & first & last obj. for each screen.
DECLARE laFisYear  [3,1]       && Array to Hold previous,current,next year
DECLARE laFileStru [1]

*** This array will hold names of the windows involved in this session 
*** on the name of first and last object in each
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'BATCHTRN'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'BATCHTRN'
laKeyField[2,4] = 2

laDefProc[7,1] = .F.            && Use local Delete procedure
laDefProc[9,1] = .F.            && Use local Save   procedure

*** Variables Decleration
lcDrOrCr    = ' '               && To Determine Namount debit or Credit 

lcSelCont   = SCHEME(1,6)           
lcEnbCont   = SCHEME(1,2) 
lcDisCont   = SCHEME(1,10) 

lcAutDes    = SPACE(40)        && Description for automatic template      
lcHoldStat  ='DISABLE'         && Status for Hold push button 
lcAcctCode  = REPLICATE("0",lnAcsSegSz)   && Default for Account code  
lcPrompt    = IIF(_DOS,"PROMPT '\<Void'",'')
lc_Title    = "Transaction details" && CHild window title
lcADJStat   = ' '
lcSourcJor  = ' '               && To display source jornal discription
lcDelMesag  = 'void'            && To be displaied in the delete message
lcAutBase   = " "               && Variable to hold templatte base code
lc_TmpTrDt  = " "               && Variable to hold temp. file name
lcAcctDesc  = " "               && Variable to display account description
lcCodeType  = "A"               && Variable to hold 1st account type  
lcPrdYear   = " "               && Variable to hold the current period year
lcPeriod    = ' '
lcOldAcct   = " "               && Variable to hold old account code
lcOldAcct1  = " "               && Variable to hold old account if from button
lcOldCode   = " "               && Variable to hold old templet code
lcHold      = 'DISABLE'         && Variable to hold cbHold if disabled/enabled 
lc_TrnDet   = " "               && Variable to hole the list string
lcVewStat   = ''                && Status for push button New transaction
lctempStat  ='DISABLE'          && Status for New template code     
lcBStamp    = ' '               && Stamp for last add user
lcObjStatus = ' ' 
laTStatus   = ' '	            && status array init.
lnOldDr     = 0                 && Variable to hold old debit  value
lnOldCr     = 0                 && Variable to hold old credit value
lnDebit     = 0                 && Variable to accept debit  value
lnCredit    = 0                 && Variable to accept cridit value
lnOldTotDr  = 0                 && Variable to hold old total debit
lnOldTotCr  = 0                 && Variable to hold old total credit
lnTranBalD  = 0                 && Variable to hold balance for debit   
lnTranBalC  = 0                 && Variable to hole balance for credit 
lnOldRec    = 0                 && Variable to hold recno to edit
lnAmount    = 0                 && Variable to hold amount to be distributed
lnTrLines   = 0                 && Variable to mantain no of enteris 
lntransNo   = 0                 && Variable to tell if the tran. changed
lnTotalDr   = 0                 && Variable to display total debit
lnTotalCr   = 0                 && Variable to display total credit
lnFcount    = 0                 && Variable to hold no. fields of entry file
lnBalance   = 0                 && Variable to hold balance 
lnSourcNo   = 1
laFisYear   =  " "              && Array to hold Pervious,current,next year       
puSourcJor  =  1                 
pbActBrow   =  1                  
llNew       = .F.               && flag for new transaction 
llFirstlog  = .T.               && flag for first time log
lltemplete  = .F.                 
llFromBton  = .F.                
llValidDat  = .T.
llBrowse    = .F.
*llglobShow  = .F.
ldOldTDate  = {}                && Variable to hold old transaction date
laSourcJor  = ' '                  
cbHold      = 0                      
*******
lcObjState  = ''
lcPostSt    = ''
lcNewSt     = ''
lcRemSt     = ''
lcAdjSt     = ''
lcDrCrSt    = ''
lcAccSt     = ''
lcOthSt     = ''
*******

*** Calling the setup function in the first time will inialize all the
*** Variables which have a default valu, in second time to enter the
*** screen it will restore back the old values of all variables

IF !gfSetup()    
  RETURN
ENDIF  

*E301077,1 Open GLSETUP [BEGIN]
=gfOpenFile(gcDataDir+'GLSETUP','','SH')
*E301077,1 [END..]

*B800691,1 M.H Begin.
*B600930,1 Hesham (Start)
*B600930,1 check for the allowing single transactions in the glsetup file
*B600930,1 if ! allowed then display message and return
*IF !glSetup.lSetSinTn
*  =gfModalGen("TRM02246B00000","DIALOG")
*  glQuitting  = .T.  
*  RETURN   
*ENDIF
*B600930,1 Hesham (End)
*B800691,1 M.H End.

llDoLocal  = .T.
lcLoclShow = "lpShow"
*lcLoclShow = "lfCheckRec"

IF TYPE('pcBatchno') = 'C'   
  lcBatchno  = pcBatchno
ENDIF

IF TYPE('pcVewMode') = 'C'
  lcVewMode  = pcVewMode
ENDIF

IF TYPE('pcEntOrTr')  ='C'
  lcEntOrTr  = pcEntOrTr
ENDIF  

*********
IF TYPE('lcBatchno') ='C'
  llFromBat = .T.
  *B602801,1 
  =SEEK(lcBatchno, 'GLBATCH')
  *B602801,1 
ELSE
  lcBatchno = '000000'
  llFromBat = .F.
  *B800691,1 M.H Begin.
  IF !glSetup.lSetSinTn
    =gfModalGen("TRM02246B00000","DIALOG")
    glQuitting  = .T.  
    RETURN   
  ENDIF
  *B800691,1 M.H End.
ENDIF  

*** Check if edit is permiter or not
IF TYPE('lcVewMode')='C'
  llViewMode = IIF(lcVewMode='T',.T.,.F.)
ELSE
  llViewMode = .F.
ENDIF

*** Check if transaction no or entery is required
IF TYPE('lcEntOrTr')='C'

  pcTrnHdRec = INT(VAL(pcTrnHdRec))
  SELECT GLTRNSHD
  IF pcTrnHdRec <= RECCOUNT()
    GOTO pcTrnHdRec  
  ENDIF  

  pcTrnDtRec = INT(VAL(pcTrnDtRec))
  SELECT GLTRNSDT
  IF pcTrnDtRec <= RECCOUNT()
    GOTO pcTrnDtRec
  ENDIF  

  lnTrsNo = RECNO('GLTRNSHD')  
  lnEntNo = RECNO('GLTRNSDT')
ELSE
  lcEntOrTr =''
ENDIF  

SELECT GLTRNSHD
*B607109,1 ALB 08/14/2003 Enable the copy&past function when the call from Jornal batch screen [BEGIN]
*lcModal         = IIF(llFromBat,'MODAL','')   && 
lcModal         = IIF(llFromBat AND llViewMode,'MODAL','')   && 
*B607109,1 ALB 08/14/2003 Enable the copy&past function when the call from Jornal batch screen [END]
laFisYear[1,1]  = STR(lnCurr_yer-1,4) + ' - Previous'
laFisYear[2,1]  = STR(lnCurr_yer  ,4) + ' - Current'
laFisYear[3,1]  = STR(lnCurr_yer+1,4) + ' - Next'

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "IBBROWKEY"
*B601099,1 M.H 06/27/96 Begin.
*laWndObj [1,3] =  IIF(llFromBat,"PBENTRIES","PBPRINT")
laWndObj [1,3] =  "PBENTRIES"
*B601099,1 M.H 06/27/96 End.

laWndObj [2,1] = "GWCCONTRL1"
laWndObj [2,2] = "PBTOP"
laWndObj [2,3] = "PBCLS"

laWndObj [3,1] = "CWRGLTRDT"
laWndObj [3,2] = "LSTRNDET"
laWndObj [3,3] = "PBDTCLOSE"

*** Check if their is any account types in the type file
SELECT GLTYPES
GO TOP
IF EOF()
  *** The types and ranges have not ***
  *** been setup yet.  You have to  ***
  *** define the accounts type and ranges first. ***
  *** < Ok > ***
  =gfModalGen("TRM02038B00000","DIALOG")
  glQuitting  = .T.  
  RETURN 
ENDIF

*** check if the chart of accounts is created.
SELECT GLACCHAR
LOCATE
IF EOF()
  *** The chart of accounts is empty. You have to create. ***
  *** the chart of accounts first...
  *** <  Ok  > ***
  =gfModalGen("TRM02215B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

SELECT GLTRNSHD
*** Check if runinnig this program for first time
IF !WEXIST(gcBaseWind)
  SELECT  cSrcJrnl+" "+cJorlnDes,cSrcJrnl;
          FROM &gcDataDir.GLSUBJOR;
          INTO ARRAY laSourcJor

  IF ALEN(laSourcJor,1) = 1 .AND. laSourcJor[1,1] = ' '
    laSourcJor = " "
    laSourcJor[1,1] = lcSJ_Def
    laSourcJor[1,2] = lcSJ_Def
  ELSE
    IF ASCAN(laSourcJor,lcSJ_Def) = 0
      DIMENSION laSourcJor[ALEN(laSourcJor,1)+1,2]
      =AINS(laSourcJor,1)
      laSourcJor[1,1] = lcSJ_Def
      laSourcJor[1,2] = lcSJ_Def
    ENDIF  
  ENDIF
  *** Get a temp. file name
  lc_TmpTrDt = gfTempName()
  *** Creat temp file with new 2 fields to hold record no. and status
  SELECT GLTRNSDT
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
  CREATE TABLE &gcWorkDir.&lc_TmpTrDt FROM ARRAY laFileStru    

  *** Define varable used in the list as a list string
  SELECT (lc_TmpTrDt)      
  DO CASE
    CASE _DOS
      lc_TrnDet = "SUBSTR(&lc_TmpTrDt..cAcctcode,1,lnAcsSegSz)"+;
                  "+' '+SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,"+;
                  "GLACCHAR.cAcctcode,'ACCTCODE'),1,40-lnAcsSegSz)+' '+lfTrnDet()"
    CASE _WINDOWS
      *B601810,1 Change list variable
      *lc_TrnDet = "SUBSTR(&lc_TmpTrDt..cAcctcode,1,lnAcsSegSz)"+;
                  "+' '+SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,"+;
                  "GLACCHAR.cAcctcode,'ACCTCODE'),1,24-lnAcsSegSz)+' '+lfTrnDet()"
      lc_TrnDet = "SUBSTR(&lc_TmpTrDt..cAcctcode,1,lnAcsSegSz)"+;
                  "+' '+SUBSTR(LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,"+;
                  "GLACCHAR.cAcctcode,'ACCTCODE'),1,24-lnAcsSegSz)+' '+"+;
                  "IIF(&lc_TmpTrDt..cDrorcr = 'D',STR(&lc_TmpTrDt..nAmount,15,2),+;
                   SPACE(15)+STR(&lc_TmpTrDt..nAmount,15,2))"
      *B601810,1 end

  ENDCASE                   
  SELECT GLTRNSHD
  *** If comming from the batch program to view/edit spacific transaction or entry
  IF !EMPTY(lcEntOrTr)
    *** Stop on the requisted record and go to vew or edit mode
    GO lnTrsNo IN GLTRNSHD
    SCATTER FIELDS &lcScFields TO laData
    laScrMode = .F.
    IF llViewMode
      laScrMode[2] = .T.
    ELSE
      laScrMode[3] = .T.
    ENDIF    
  ELSE
    *** If not initaliz the laData array with blanks
    SCATTER FIELDS &lcScFields TO laData BLANK
  ENDIF  

  *** Calling the gpStatic procedure will creat temp record in the static file 
ELSE
   llNew       = .F.
   llFirstlog  = .F.
ENDIF

lcNewStat = IIF(laScrMode[1] .OR. laScrMode[2],"ENABLE","DISABLE")

*** Setting brows filter 
*** If comming from the menu filter on transaction with batch No. 0
*** If comming form the batch program filter on the batch No. Sent as 
*** a parameter

SELECT GLTRNSHD
SET FILTER TO (cBatchNo=lcBatchNo) AND CTRNSTAT $ 'EOUHAP'
SET RELATION TO "T"+ gltrnshd.cautcode INTO GLAUTHD ADDITIVE

llDoLocal  = .T.

IF VAL(lcBatchno) > 0 .AND. !llViewMode AND GLBATCH.CBATSTAT = 'E'
  lnCurObj = 3
  KEYBOARD "{ENTER}"
  KEYBOARD "{ENTER}"
ENDIF  

**********
lcObjStatus = IIF(laScrMode[2] .AND. laData[8] $ 'EOUZH',"ENABLE","DISABLE")
laCtrStat[7]= lcObjStatus
laCtrStat[8]= lcObjStatus
**********

*** Call the scareen program
SELECT GLTRNSHD
*E300683,5 Call *.SPR from screens directory
* DO Gltrans.SPR 

DO (gcScrDir + gcWinAppl + '\Gltrans.SPR')

*E300683,5 end  
SELECT GLTRNSHD
SET FILTER TO   
SET RELATION TO 

*** If quitting from the program release the child window
*** and erase the temp file

*B602311,1 KAM (start)

*IF glQuitting
 * IF WEXIST('CWRGLTRDT')
 *   RELEASE WINDOW CWRGLTRDT
 * ENDIF
  
 * SELECT GLTRNSHD
 * SET FILTER TO   
 * SET RELATION TO 

  ** If exit from this program, erasing all the temp & text files **
 * IF USED(lc_TmpTrDt)
  *  USE IN ALIAS(lc_TmpTrDt)
  *ENDIF
  *ERASE &gcWorkDir.&lc_TmpTrDt..DBF
*ENDIF

IF WEXIST('CWRGLTRDT')
 RELEASE WINDOW CWRGLTRDT
ENDIF
  
SELECT GLTRNSHD
SET FILTER TO   
SET RELATION TO 

** If exit from this program, erasing all the temp & text files **
IF USED(lc_TmpTrDt)
  USE IN ALIAS(lc_TmpTrDt)
ENDIF
ERASE &gcWorkDir.&lc_TmpTrDt..DBF

*B602311,1 KAM (end)
RETURN
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
*** called when ever Show Gets command is issued
PROCEDURE lpShow

SELECT GLTRNSHD
IF laScrMode [2]
  lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
ENDIF  

*** laData[1] will carry the batch no or 0
laData[1]  = lcBatchno

=lfCheckRec() 

laData[18] = gcAct_Appl 
laData[21] = gcAct_Comp 
laData[20] = "N" 

lcVewStat=IIF(llViewMode,'DISABLE','ENABLE')
*** Automatic base doller or percent have to be reset with each new recrd
lcAutBase  = ''

*** if add mode then use Exist batch no. else no change
*** We have in laStatus
*** 'E' For 'Empty'
*** 'U' For ' Balanced'
*** 'O' For 'Out Of Balance '
*** # for dummy element of other screens
 
DO CASE 
  *** Select mode
  CASE laScrMode[1]
    lnTotalDr  = 0 
    lnTotalCr  = 0
    lnTranBalD = 0                       &&  Transation Balance Dr.
    puSourcJor = 0
    lnTranBalC = 0
    lnBalance  = 0
    lcDrOrCr   = ' '
    lcAutDes   = ' '
    lcSourcJor = ' '
    ****
    lcNewSt  = 'DISABLE'
    lcRemSt  = 'DISABLE'
    lcAdjSt  = 'DISABLE'
    lcAccSt  = 'DISABLE'    
    lcDrCrSt = 'DISABLE'
    lcOthSt  = 'DISABLE'
    lcPostSt = 'DISABLE'    
    SHOW GET ladata[2]  ENABLE
    SHOW GET pbpost     DISABLE
    SHOW GET ladata[3]  DISABLE
    SHOW GET ladata[16] DISABLE    
    SHOW GET ibTempCode DISABLE        
    ****
    
    IF llFromBat .AND. llviewmode   
      SHOW GET pbNewTr   DISABLE
    ELSE
      SHOW GET pbNewTr   ENABLE
    ENDIF
      
    SHOW GET cbHold    DISABLE
    SHOW GET  puSourcJor  DISABLE    
    IF WVISIBLE('CWRGLTRDT')
      =gfChClose('CWRGLTRDT')
    ENDIF  
    SELECT (lc_TmpTrDt)
    ZAP
    SELECT GLTRNSHD
    *** Cahnge the delete botton to void
    lcObjStatus = 'DISABLE'
    SHOW GET pbDlt,1 &lcPrompt DISABLE
    *B600486,1 Control the status of the delete bar in the record pad.
    laCtrStat[8] = "DISABLE"

    =lfRefresh()
  *** View mode
  CASE laScrMode[2] .OR. laScrMode[3]
    lcAutDes = GLAUTHD.cAutDes
    =lfRefresh()    
    IF llFirstlog
      *** Get the right discription of the source jornal code comming from
      *** the transaction header record
    lcExactSet = SET("EXACT")
    IF !EMPTY(laSourcJor)
      SET EXACT ON
      lnSourcNo  = ASCAN('laSourcJor',laData[11])
      IF lnSourcNo > 0
        lcSourcJor = laSourcJor [ASUBSCRIPT('laSourcJor',lnSourcNo,1),1]
      ELSE
        lcSourcJor = ' '
        lnSourcNo  = ASCAN('laSourcJor',lcSJ_Def)
      ENDIF
      puSourcJor = ASUBSCRIPT('laSourcJor',lnSourcNo,1)
      SET EXACT &lcExactSet
    ENDIF
    *** Collect all transaction lines for this header
    SELECT *,RECNO() AS 'nRecNo' , "S" AS 'cStatus';
            FROM &gcDataDir.GLTRNSDT ;
            INTO DBF &gcWorkDir.&lc_TmpTrDt;
            WHERE GLTRNSDT.cBatchno+GLTRNSDT.cTranno = laData[1]+laData[2]
    lnTrLines  = _TALLY
    lsTrnDet   = 1    
    *** Get the type of accounts
    lnTotalDr  = MAX( laData[9], laData[10])
    lnTotalCr  = lnTotalDr
    lnTempBal  = ABS(laData[9] - laData[10])
    lnBalance  = lnTempBal
    lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)
    lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                   GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
    lcAcctCode = &lc_TmpTrDt..cAcctcode
    lcAcctDesc = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,;
                        GLACCHAR.cAcctcode,'ACCTCODE')
    IF &lc_TmpTrDt..cDrOrCr ="D"
      lnDebit  = &lc_TmpTrDt..nAmount
      lnCredit = 0
    ELSE
      lnDebit  = 0
      lnCredit = &lc_TmpTrDt..nAmount
    ENDIF

    *B601449,1 This line was added by HS to fix the Comment field [Begin]
    lcTrdTexp = &lc_TmpTrDt..cTrDtExp
    *B601449,1 This line was added by HS to fix the Comment field [End]
    
    IF llFromBat
      lnOldTotDr = laData[9]
      lnOldTotCr = laData[10]
    ENDIF            
    cbHold     = IIF(laData[8]='H',1,0)
    lcHold     = IIF(laData[8] $ 'UH','ENABLE','DISABLE') 
    lctempStat = IIF(lnTrLines= 0    ,'ENABLE','DISABLE')
    lcADJStat  = IIF(laData[9]=laData[10],'DISABLE','ENABLE')
    lcAdjSt = lcADJStat
    lcOthSt = lcTempStat
  ELSE
    llFirstlog  = .T.
  ENDIF
    IF laScrMode[2]
      SHOW GET ladata[3]  DISABLE
      SHOW GET ladata[16] DISABLE    
      SHOW GET ibTempCode DISABLE
    *B600552,1 (start) Disabling the Get fields of the entries screen  in the view mode
      SHOW GET lnCredit DISABLE      &&  Cridet
      SHOW GET lnDebit DISABLE       &&  Debit
      SHOW GET lcAcSegDes DISABLE    &&  Account Segment Description
      SHOW GET lcAcctCode DISABLE    &&  Account Code
      SHOW GET lcAcctDesc DISABLE    &&  Account Description
      SHOW GET lcTrdTexp DISABLE
    *B600552,1 (end)
      SHOW GET  puSourcJor     DISABLE              
      IF laData[8] $ 'PA'
        lcPostSt = 'DISABLE'
        SHOW GET pbPost &lcPostSt
        SHOW GET pbEdt  DISABLE
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "DISABLE"
        
        SHOW GET pbCopy DISABLE 
        lcObjStatus = 'DISABLE'     
        SHOW GET pbDlt,1 &lcPrompt DISABLE
        *B600486,1 Control the status of the delete bar in the record pad.
        laCtrStat[8] = "DISABLE"
        
      ELSE
        IF laData[19]='Y'
          lcPostSt   = IIF(laData[8]='U','ENABLE','DISABLE') 
          SHOW GET pbPost &lcPostSt
        ELSE
          lcPostSt   = IIF(laData[8] $ 'UO','ENABLE','DISABLE') 
          SHOW GET pbPost &lcPostSt
        ENDIF
        IF llFromBat   
          IF llViewMode  
            lcObjStatus = 'DISABLE'
            SHOW GET pbDlt,1 &lcPrompt DISABLE
            *B600486,1 Control the status of the delete bar in the record pad.
            laCtrStat[8] = "DISABLE"
            
            SHOW GET pbEdt,1  DISABLE
            *B600486,1 Control the status of the edit bar in the record pad.
            laCtrStat[7] = "DISABLE"
          ELSE
            lcObjStatus = 'ENABLE'
            SHOW GET pbDlt,1 &lcPrompt ENABLE
            *B600486,1 Control the status of the delete bar in the record pad.
            laCtrStat[8] = "ENABLE"
            
            SHOW GET pbEdt,1  ENABLE 
            *B600486,1 Control the status of the edit bar in the record pad.
            laCtrStat[7] = "ENABLE"
          ENDIF   
        ELSE
          lcObjStatus = 'ENABLE'
          SHOW GET pbDlt,1 &lcPrompt ENABLE
          *B600486,1 Control the status of the delete bar in the record pad.
          laCtrStat[8] = "ENABLE"
          SHOW GET pbEdt,1  ENABLE 
          *B600486,1 Control the status of the delete bar in the record pad.
          laCtrStat[7] = "ENABLE"
        ENDIF
      ENDIF

      SHOW GET laData[2]    DISABLE
      SHOW GET laData[3]    DISABLE
      lcOthSt = 'DISABLE'
      SHOW GET laData[4]    DISABLE
      SHOW GET laData[5]    DISABLE
      SHOW GET laData[6]    DISABLE
      SHOW GET laData[7]    DISABLE
      SHOW GET laData[12]   DISABLE
      SHOW GET laData[16]   DISABLE
      SHOW GET ibTempCode   DISABLE              
      SHOW GET pbNewTr      &lcVewStat
      SHOW GET pbEntries    ENABLE
      SHOW GET pbPrint      ENABLE
      SHOW GET cbHold       DISABLE
      SHOW GET pbAdj        DISABLE

      lcNewSt  = 'DISABLE'
      lcRemSt  = 'DISABLE'
      lcOthSt  = 'DISABLE'
      lcAdjSt  = 'DISABLE'

      *** Cahnge the delete botton to void
   ELSE 
     lcstat  = IIF(lnTrLines = 0,"DISABLE","ENABLE")
     SHOW GET pbRem      &lcStat
     lcRemSt = lcStat
     lcOthSt  = IIF(lnTrLines <> 0,"DISABLE","ENABLE")

      SHOW GET  puSourcJor  ENABLE
      SHOW GET laData[3 ]  &lctempStat
      SHOW GET laData[16]  &lctempStat
      SHOW GET ibTempCode  &lctempStat
      lcOthSt = lctempStat
      SHOW GET pbNewTr    DISABLE
      SHOW GET pbPrint    DISABLE
      lcPostSt = 'DISABLE'
      SHOW GET pbPost &lcPostSt
      SHOW GET cbHold     &lcHold
      SHOW GET pbAdj      &lcADJStat 
      lcAdjSt = lcADJStat
      *** Cahnge the delete botton to void
      lcObjStatus = 'DISABLE'
      SHOW GET pbDlt,1 &lcPrompt DISABLE
      *B600486,1 Control the status of the delete bar in the record pad.
      laCtrStat[8] = "DISABLE"
   ENDIF
  =lfControl()
  *** Add mode 
  CASE laScrMode[4]
  
    IF llFirstlog  .OR. llNew
      llNew=.F.
      lnAmount   = 0
      lnTotalDr  = 0 
      lnTotalCr  = 0
      puSourcJor = 0
      lnTranBalD = 0                       &&  Transation Balance Dr.
      lnTranBalC = 0
      lnTrLines  = 0
      lnDebit    = 0 
      lnCredit   = 0 
      lnBalance  = 0
      lcAutDes   = ' '
      lcCodeType = "A"
      lcAutBase  = ''    
      lcAcctCode = REPLICATE("0",lnAcsSegSz) 
      lcAcctDesc = ""
      lcDrOrCr   = ''
      laData[2]  = SPACE(FSIZE(glTrnsHd.ctranNo))
      laData[8]  = 'E'             && init. status with 'E'==empty
      lcExactSet = SET("EXACT")
      SET EXACT ON
    
      lnSourcNo  = ASCAN('laSourcJor',lcSJ_Def)
      puSourcJor = ASUBSCRIPT('laSourcJor',lnSourcNo,1)
      IF lnSourcNo > 0
        lcSourcJor = laSourcJor [ASUBSCRIPT('laSourcJor',lnSourcNo,1),1]
        laData[11] = laSourcJor [ASUBSCRIPT('laSourcJor',lnSourcNo,1),2]
      ELSE
        lcSourcJor = ' '
      ENDIF

      SET EXACT &lcExactSet
      laData[15] = 'N'             && reverse 
      laData[5]  = SUBSTR("On " + IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                         LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))+SPACE(15),1,15)
      laData[3]   = gdSysDate
      ldOldTDate  = {}
      SHOW GET laData[3]     
      laData[4]  = "Created by " + LOOKUP(SYUUSER.cUsr_Name,gcUser_Id,SYUUSER.cUser_Id)
      laData[4]  =SUBSTR(laData[4],1,40)
      
      SHOW GET laData[4]     
      *** We have to close the chils window in case of adding new transaction
      *** till the user add the transaction date
      IF WVISIBLE('CWRGLTRDT')
        =gfChClose('CWRGLTRDT')
      ENDIF  
      SHOW GET pbAdj       DISABLE
      lcAdjSt = 'DISABLE'      
      SHOW GET pbNewTr     DISABLE
      SHOW GET pbPrint     DISABLE
      lcPostSt = 'DISABLE'
      SHOW GET pbPost &lcPostSt
      SHOW GET pbRem       DISABLE
      lcRemSt = 'DISABLE'
      lcOthSt = 'ENABLE'
      SHOW GET lcAcctCode  DISABLE
      
      *B600390,1 Refresh the acc. browse invisable button with every account
      *B600390,1 refreshing in the different modes.
      SHOW GET ibActBrow   DISABLE
      
      lcAccSt  = 'DISABLE'    
      lcDrCrSt = 'DISABLE'
      SHOW GET lnDebit     DISABLE
      SHOW GET lnCredit    DISABLE
      SHOW GET lcTrdtexp   DISABLE
      SHOW GET cbHold      DISABLE
      SHOW GET  puSourcJor ENABLE      
      *** Cahnge the delete botton to void
      lcObjStatus = 'DISABLE'      
      SHOW GET pbDlt,1 &lcPrompt DISABLE
      *B600486,1 Control the status of the delete bar in the record pad.
      laCtrStat[8] = "DISABLE"
      SHOW GET ibActBrow  DISABLE
      SHOW GET pbActBrow  DISABLE
    ELSE
      SHOW GET pbNewTr     DISABLE
      SHOW GET pbPrint     DISABLE
      lcPostSt = 'DISABLE'
      SHOW GET pbPost &lcPostSt
      llFirstlog  = .T.  
      llNew       = .F.
    ENDIF
 ENDCASE
*** If comming from the batch program with spesific transaction entry
*** select this entry and activat the child window
IF lcEntOrTr = 'E'
  lcEntOrTr =''
  =lfControl()
  =gfActWind('CWRGLTRDT',lc_Title)
  SELECT (lc_TmpTrDt)
  LOCATE FOR nRecNo = lnEntNo
  lsTrnDet = RECNO()
  *** If the transaction is balanced select the list to stop on 
  IF laData[9] = laData[10]
    _CUROBJ = OBJNUM(lsTrnDet)
  ELSE
    *** If not balanced stop on the debit or credit fields to modify
    IF cDrOrCr = 'D'
      _CUROBJ = OBJNUM(lnDebit)
    ELSE
      _CUROBJ = OBJNUM(lnCredit)
    ENDIF
    *** Call the list when funcion to update the totals and account fields
    =lfwTrnDet()
  ENDIF  
ENDIF

*** All Controle invisible buttons has to be active all the time
SHOW GET ibBgDumi1   ENABLE
SHOW GET ibEndDumi1  ENABLE
SHOW GET ibFrstObj   ENABLE
SHOW GET ibLastObj   ENABLE

*** These objects is for display only
SHOW GET laData[7]   DISABLE
SHOW GET laData[6]   DISABLE
SHOW GET lnTranBalD  DISABLE
SHOW GET lnTranBalC  DISABLE
SHOW GET lnTotalDr   DISABLE
SHOW GET lnTotalCr   DISABLE
cbHold     = IIF(laData[8]='H',1,0)
IF laScrMode[3] .OR. laScrMode[4]
  lcHold     = IIF(laData[8] $ 'UH','ENABLE','DISABLE') 
  SHOW GET cbHold     &lcHold
ELSE
  SHOW GET cbHold     DISABLE
ENDIF
IF !EMPTY(laSourcJor[1,1])
  SHOW GET ibPopup     COLOR ,,,,,&lcSelCont,,,&lcEnbCont,&lcDisCont
ELSE
  SHOW GET ibPopup DISABLE
ENDIF
*** Refresh the list object

IF laScrMode[3] .OR. laScrMode[4]
  lctempStat = IIF(lnTrLines= 0    ,'ENABLE','DISABLE')
  SHOW GET laData[3 ] &lctempStat
  SHOW GET laData[16] &lctempStat
  SHOW GET ibTempCode &lctempStat  
  lcOthSt = lctempStat
ENDIF  

llBrowse  = .F.
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
* Validation of the transaction no key field
*B600392,1 Make 2 cases of validation :_
*B600392,1   _ If coming from the menu.  _ If coming from the batch screen.
*
FUNCTION lfvData_2

*B602803,1 [BEGIN] 
IF MDOWN()
  RETURN
ENDIF
*B602803,1 [END..]  

SELECT GLTRNSHD

IF llBrowse
  *B601449,1 Change this line to fix the browse when coming from 
  *B601449,1 the Batch screen [Begin]
  *laData[1] = '?'
  laData[2] = '?'
  *B601449,1 Change this line [End]
ENDIF

*B600392,1 In case of coming from the view mode in the batch screen.
IF llFromBat .AND. llviewmode
  *B600392,1 If type "?" in the key field.
  IF LEFT(laData[2],1) = '?'
    *B600392,1 Restore the old value of the key field.
    lcOldData = laData[2]
    *B600392,1 Browse only the selected batches transactions
    =gfBrows('laData[1]',lcScFields,'laData')
    *B600392,1 Compare the key field with its old value after coming
    *B600392,1 from the browse to know if return with select or cancel.
    IF laData[2] <> lcOldData
      *B600392,1 If coming with selecting record, go to the view 
      *B600392,1 mode to refresh the data
      laScrMode    = .F.
      laScrMode[2] = .T.       && Go to view mode
      SHOW GETS
    ELSE
      *B600392,1 If coming with a cancel from the browse, clear the
      *B600392,1 key field to back to select mode.
      laData[2] = ""
    ENDIF
  ELSE
  
    *B600392,1 Add zeros to the key field to display it in the message.
    laData[2]=RIGHT("00000000"+ALLTRIM(laData[2]),8)
  *B600513,1 Search for the transaction  
  IF SEEK(laData[1]+laData[2])
     *B600513,1 If found change the screen mode, scatter the data, and refresh the screen
     laScrMode    = .F.
     laScrMode[2] = .T.
     SCATTER FIELDS &lcScFields TO laData
     SHOW GETS
  ELSE  
    *B600513,1 If not found give the user the option to renter or browse
    *Messsage : " ð is not found in the data file. "
    *Button   : "  < Browse > - < Reenter > "
    IF gfModalGen("QRM00001B00014","DIALOG","GL Batch Number\Transaction Number : "+laData[1]+laData[2]) = 1
      *B600392,1 If browse, save the old key field value.
      lcOldData = laData[2]
      *B600392,1 Browse the current batch transactions.
      =gfBrows('laData[1]',lcScFields,'laData')
      *B600392,1 Compare the key field with its old value after coming
      *B600392,1 from the browse to know if return with select or cancel.
      *B600513,1 if data is choosen from browse
      IF laData[2] <> lcOldData
        *B600392,1 If coming with selecting record, go to the view 
        *B600392,1 mode to refresh the data
        laScrMode    = .F.
        laScrMode[2] = .T.       && Go to view mode
        SHOW GETS
      ELSE
        *B600392,1 If coming with a cancel from the browse, clear the
        *B600392,1 key field to back to select mode.
        laData[2] = ""
      ENDIF
    ELSE
      *B600392,1 If press reenter in the dialog box, clear the key field 
      *B600392,1 to back to select mode.
      laData[2] = ""
    ENDIF
  ENDIF
  ENDIF
ELSE
*-------------
  *B600392,1 If branching from the menu.
  IF llBrowse .OR. (!EMPTY(laData[2]) .AND. LASTKEY() = 13)
    IF LEFT(laData[2],1) <> '?' 
      laData[2]=RIGHT("00000000"+ALLTRIM(laData[2]),8)
      SHOW GET laData[2] DISABLE
    ELSE
      laData[1] = '?'
      laData[2] = ''
    ENDIF  
     
    IF RECCOUNT() <> 1 .AND. RECNO() <= RECCOUNT()
      GOTO RECNO()  
    ENDIF 
    =gfSeekRec()
    
    IF laScrMode[1]
      _CUROBJ = OBJNUM(laData[2])
    ENDIF

    *B600392,1 Remark this few lines & separate the case of branching from 
    *B600392,1 the view mode in the batch screen.
*    IF llFromBat .AND. llviewmode .AND. laScrMode[4]
*      laScrMode = .F.
*      laScrMode[1] = .T.
*      SHOW GETS
*    ENDIF
  ENDIF
ENDIF

IF laScrMode[1]
  SHOW GET laData[2]  ENABLE && Malak 
ENDIF
llBrowse = .F.

*!**************************************************************************
*!
*!      Function: lfvAddTrn
*!
*!**************************************************************************
*
* Adding new tranaction push button
*
FUNCTION lfvAddTrn
llNew=.T.

IF laScrMode[2]
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcAcctDesc = ""
  lcTrdtexp  = ""
  IF WVISIBLE('CWRGLTRDT')
    =gfChClose('CWRGLTRDT')
  ENDIF  
  SHOW GET LSTRNDET DISABLE
  SELECT (lc_TmpTrDt)
  ZAP
  SELECT GLTRNSHD
ENDIF  

laScrMode     = .F.              
laScrMode[4]  = .T.                           && Go To Add Mode

SELECT GLTRNSHD
SHOW GETS 

*!**************************************************************************
*!
*!      Function: lfvVldDate
*!
*!**************************************************************************
*
* Transaction date  Or reverse date validation
*

FUNCTION lfvVldDate

lcObjName  = SYS(18)
ldChekDate = EVALUATE(SYS(18))
lcTrOrRev  = IIF(lcObjName='LADATA(3)','Transaction','Reverse')

llValidDat = .T.
*** Get the period of the sending date & its ***
*** begining date & ending date...  
lnPeriod  = 0
lcPrdYear = ''
llLockSt  = .F.
lnPeriod  = gfPeriod(ldChekDate,gcPrnt_Cmp,@lcPrdYear,'','',@llLockSt)
lcPeriod  = RIGHT("0"+ALLTRIM(STR(lnPeriod)),2)
IF llFromBat .AND. lcObjName='LADATA(3)'
  IF !BETWEEN(ldChekDate,glbatch.dbatpbeg,glbatch.dbatpend) 
    =gfModalGen("TRM02010B00000","Dialog",DTOC(glbatch.dbatpbeg)+'   '+DTOC(glbatch.dbatpend))
    llValidDat = .F.
    =lfVldDate()      
    RETURN      
  ENDIF
ENDIF

IF (!EMPTY(laData[12]) .OR. lcObjName='LADATA(3)') .AND. llValidDat
  DO CASE
    CASE !BETWEEN(VAL(lcPrdYear),lnCurr_yer-1,lnCurr_yer+1)
      *** Transaction/Revers date must fall within the posting window
      =gfModalGen("TRM02011B00000","Dialog",lcTrOrRev)
      llValidDat = .F.
      =lfVldDate()      
      RETURN      
    CASE lnPeriod > 0 .AND. llLockSt
      *** This date fall in a locked period
      =gfModalGen("TRM02158B00000","Dialog")
      llValidDat = .F.
      =lfVldDate()      
      RETURN      
    ENDCASE
 
  IF ldChekDate < glSetup.dSetBBDat
    IF glSetup.lSetPBBBd 
      *** Transaction/Reverce date must not fall before the Beginning balance date ---
      =gfModalGen("TRM02159B00000","Dialog",lcTrOrRev+"|"+DTOC(glSetup.dSetBBDat))
      llValidDat = .T.
      =lfVldDate()      
      RETURN           
    ELSE
      *** Transaction/Reverce date must not fall before the Beginning balance date ---
      *** Posting before the Beginning balance date is not allowed.
      =gfModalGen("TRM02202B00000","Dialog",lcTrOrRev+"|"+DTOC(glSetup.dSetBBDat))
      llValidDat = .F.    
      =lfVldDate()      
      RETURN      
    ENDIF  
  ELSE
    =lfVldDate()      
  ENDIF
*B801455,1 AMM start, call this function when the reverse date is empty to 
*B801455,1 AMM set the proper value in the reverse period and year.
ELSE
  =lfVldDate()      
*B801455,1 AMM end
ENDIF

*!**************************************************************************
*!
*!      Function: lfVldDate
*!
*!**************************************************************************
*
* Transaction date  Or reverse date validation
*

FUNCTION lfVldDate
IF llValidDat
  IF lcObjName='LADATA(3)'  
    laData[6] = lcPrdYear
    laData[7] = lcPeriod
    SHOW GET laData[6] DISABLE
    SHOW GET laData[7] DISABLE
    IF lnTrLines > 0
       SELECT (lc_TmpTrDt)
       REPLACE ALL dTrnpDate WITH laData [3] ;
                   cTrnpYr   WITH laData [6] ;
                   cTrnpPrd  WITH laData [7] ;
                   cStatus   WITH SUBSTR("MAM",AT(cStatus,"MAS"),1)
    ENDIF      
    IF lcPeriod+lcPrdYear <> ALLTRIM(lcCurr_prd)+ALLTRIM(lccurr_yer) 
      *** Transaction date dose not belong to the current period
      =gfModalGen("TRM02027B00000","Dialog")
    ENDIF
  ELSE
    IF EMPTY(laData[12])
      laData[15]='N'
      laData[13]=' '
      laData[14]=' '
    ELSE
      laData[15]='Y'
      laData[13]=lcPrdYear
      laData[14]=lcPeriod
    ENDIF 
  ENDIF
ELSE
  &lcObjName = ldOldTDate
  _CUROBJ   = OBJNUM(&lcObjName) 
  SHOW GET &lcObjName
ENDIF

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
* Templete code validation
*
FUNCTION lfvData_16

*B600549,1 We should check  on not empty not val = 0
*IF !llBrowse .AND. VAL(laData[16]) <> 0
IF !llBrowse .AND. !EMPTY(laData[16])
  *B600549,1 We should not Pad by zeros just alltrim the field
  * laData[16]=RIGHT("00000000"+ALLTRIM(laDaTa[16]),8)
  laData[16] = PADR(ALLTRIM(laDaTa[16]),8)
ENDIF

IF llBrowse .OR. (laData[16] <> lcOldCode .AND. !EMPTY(laData[16])) 
  laData[19]=IIF(llFromBat,IIF(GLBATCH.cBatType='S','N','Y'),' ')
  SELECT GLAUTHD
  lcOldTag=SYS(22)
  SET ORDER TO TAG TYPECODE
  SET ORDER TO TAG TYPCODACC IN GLAUTDT
  SET RELATION TO glauthd.cauttype + glauthd.cautcode INTO GLAUTDT ADDITIVE

  SELECT GLAUTDT
  SET ORDER TO TAG ACCTCODE  IN GLACCHAR
  SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE

  SELECT GLAUTHD
  IF SEEK('T')
    IF llBrowse .OR. !SEEK('T'+laData[16]) .OR. ATC("?",laData[16]) > 0
      DECLARE laRetInfo  [2]
      IF RECNO(0) > 0 .AND. RECNO(0) < RECCOUNT("GLAUTHD")
         GO RECNO(0)
      ELSE
        GO TOP
      ENDIF
      lcSaveBrow       = lcBrFields
      lcBrFields       = "cAutCode:H='Automated entry code',cAutDes:H='Description'"
      laRetInfo[1]     = laData[16]
      lcSavFTitl       = lcFile_Ttl
      lcFile_Ttl       = 'Automated code file' 
       =gfBrows(["T"],'cAutCode,cAutDes',"laRetInfo",.T.)  
      lcBrFields       = lcSaveBrow
      lcFile_Ttl       = lcSavFTitl

      IF laData[16] <> laRetInfo[1]
         laData[16] = laRetInfo[1]
         lcAutDes   = laRetInfo[2]
      ELSE
         laData[16] = lcOldCode
      ENDIF
    ENDIF
  ELSE
    =gfModalGen("TRM00052B00000",'DIALOG')
    SELECT GLAUTHD
    SET ORDER TO TAG &lcOldTag
    SET RELATION TO
    SELECT GLAUTDT
    SET RELATION TO 
    laData[16] = lcOldCode
    RETURN    
  ENDIF
  *** Check all accounts in the template are active and allowed for posting
  llValidtmp = .T.
  SELECT  GLAUTDT
  SCAN REST WHILE cauttype + cautcode = 'T'+glauthd.cautcode
    DO CASE
      CASE GLACCHAR.cSegActiv='I'
        *** This templet has one or more inactiv account 
        =gfModalGen("TRM02164B00000",'DIALOG','template')
        llValidtmp = .F.
        EXIT
      CASE GLACCHAR.cSegAlPos='N'
        *** This templet has one or more account(s) not allowed for posting
        =gfModalGen("TRM02163B00000",'DIALOG','template')
        llValidtmp = .F.
        EXIT
      CASE llFromBat .AND. GLACCHAR.cStandard <> laData[19]
        IF laData[19]='N'  
          =gfModalGen("TRM02030B00000",'DIALOG',GLACCHAR.cAcctCode )
        ELSE
          =gfModalGen("TRM02031B00000",'DIALOG',GLACCHAR.cAcctCode )
        ENDIF
        llValidtmp = .F.
        EXIT
     ENDCASE  
  ENDSCAN
  SET RELATION TO
  IF !llValidtmp 
    laData[16] = lcOldCode  
    _CUROBJ = OBJNUM(laData[16])
  ELSE
     SELECT GLAUTHD
     IF !EMPTY(laData[16])
       lcAutDes  =GLAUTHD.cAutDes
       =lfRefresh()     
     ENDIF
     lcAutBase = GLAUTHD.cAutBase
  ENDIF

  SHOW GET laData[3 ]
  SHOW GET laData[16]
  SHOW GET lcAutDes
  SELECT GLAUTHD
  SET ORDER TO TAG &lcOldTag
  SET RELATION TO
  
  IF WVISIBLE("CWRGLTRDT")
    IF EMPTY(laData[3])
      *** You have to enter transaction date  first
      =gfModalGen( "TRM02015B00000" , "Dialog")
      _CUROBJ = OBJNUM(laData[3])
    ELSE
      *** If the user add valid new template code in the add mode
      IF !EMPTY(laData[16]) .AND. lnTrLines = 0 .AND. (laScrMode[4] .OR. laScrMode[3])
        *** Generat automatic transaction details and activate child window
        IF lfvTemp() 
          lcOldCode = laData [16]
          =lfControl()
          IF (VAL(STRTRAN(lcAcctCode,'-','')) = 0  .AND. lnTrlines <> 0 )
            SHOW GET pbNew      DISABLE
            lcNewSt = 'DISABLE'
          ENDIF
          =gfActWind('CWRGLTRDT',lc_Title)
          lnAmount=0
          =lfStat()
        ENDIF  
      ELSE
        *** If vew or edit existed lines
           =lfControl()
           IF (VAL(STRTRAN(lcAcctCode,'-','')) = 0  .AND. lnTrlines <> 0 )
             SHOW GET pbNew      DISABLE
             lcNewSt = 'DISABLE'
           ENDIF
          =gfActWind('CWRGLTRDT',lc_Title)
      ENDIF
    ENDIF   
  ENDIF
ELSE
  IF EMPTY(laData[16])
    lcAutDes  =' '
    =lfRefresh()     
  ENDIF
ENDIF

llBrowse    = .F.
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function:  lfwPopup
*!
*!**************************************************************************
*
*
FUNCTION  lfwPopup

IF !WEXIST('awrGltrans')
  SELECT  cSrcJrnl+" "+cJorlnDes,cSrcJrnl;
          FROM &gcDataDir.GLSUBJOR;
          INTO ARRAY laSourcJor
  IF ALEN(laSourcJor,1) = 1 .AND. laSourcJor[1] = ' '
    laSourcJor = " "
    laSourcJor[1,1] = lcSJ_Def
    laSourcJor[1,2] = lcSJ_Def
  ELSE
    IF ASCAN(laSourcJor,lcSJ_Def) = 0
      DIMENSION laSourcJor [ALEN(laSourcJor,1)+1,2]
      =AINS(laSourcJor,1)
      laSourcJor[1,1] = lcSJ_Def
      laSourcJor[1,2] = lcSJ_Def
    ENDIF  
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function:  lfvPopup
*!
*!**************************************************************************
*
* Source jornal validation
*
FUNCTION  lfvPopup

DO CASE
  CASE _DOS
    laData[11] = ALLTRIM(gfActPop(8,20,IIF(ALEN(laSourcJor,1) > 8,16,;
                 ALEN(laSourcJor,1)+10),65,'laSourcJor',2,1,@lcSourcJor))
  CASE _WINDOWS
    laData[11] = ALLTRIM(laSourcJor[puSourcJor,2])
ENDCASE  

=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
* Hold transaction check box validation
* This object will only be displaied
FUNCTION lfvHold

laData[8] = IIF(cbHold=1,'H','U')
=lfRefresh()

*!**************************************************************************
*!
*!      Function: lfvEntries
*!
*!**************************************************************************
*
*

FUNCTION lfvEntries

IF EMPTY(laData[3])
  *** You have to enter transaction date  first
  =gfModalGen( "TRM02015B00000" , "Dialog")
  _CUROBJ = OBJNUM(laData[3])

ELSE
  IF ! lfCheckRec()
    RETURN
  ENDIF  

  *** If the user add valid new template code in the add mode
  IF !EMPTY(laData[16]) .AND. lnTrLines = 0 .AND. (laScrMode[4] .OR. laScrMode[3])
    *** Generat automatic transaction details and activate child window

    IF lfvTemp() 
      lcOldCode = laData [16]
      =lfControl()
      IF (VAL(STRTRAN(lcAcctCode,'-','')) = 0  .AND. lnTrlines <> 0 )
        SHOW GET pbNew      DISABLE
        lcNewSt = 'DISABLE'        
      ENDIF
      =gfActWind('CWRGLTRDT',lc_Title)
      lnAmount=0
      =lfStat()
    ENDIF  
  ELSE
    *** If vew or edit existed lines
     =lfControl()
       IF (VAL(STRTRAN(lcAcctCode,'-','')) = 0  .AND. lnTrlines <> 0 )
        SHOW GET pbNew      DISABLE
        lcNewSt = 'DISABLE'        
      ENDIF
      =gfActWind('CWRGLTRDT',lc_Title)
  ENDIF
ENDIF   
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Procedure: lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost

IF ! lfCheckRec()
  RETURN
ENDIF  

IF ASCAN(laFisYear,glTrnsHd.cTrnpyr) <> 0
  IF gfModalGen("TRM02149B02009","DIALOG",'transaction') = 1 
     SELECT GLTRNSHD
     IF gfObj_Lock(.T.)
       IF ! lfCheckRec()
         RETURN
       ENDIF 
       IF lfTBPost("Transaction") > 0 
         laScrMode = .F.
         laScrMode [1] = .T.   
         SHOW GETS            
       ENDIF
       =gfObj_Lock(.F.)  
     ENDIF
  ENDIF      

  IF WVISIBLE('CWRGLTRDT')
    =gfChClose('CWRGLTRDT')
  ENDIF  
ELSE   
  ***  The Transaction posting year is out of the posting window.    
  =gfModalGen("TRM02147B00000","Dialog","transaction|transaction")     
ENDIF                        
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Procedure: lfvPrint
*!
*!**************************************************************************
*
*** 

FUNCTION lfvPrint
IF ! lfCheckRec() 
  RETURN
ENDIF  

*!**************************************************************************
*!                             Child window validation
*!**************************************************************************

*!**************************************************************************
*!
*!      Function: lfwTrnDet
*!
*!**************************************************************************
* 
*    WHEN function for the list "lsTrnDet" 
*
FUNCTION lfwTrnDet

*** IF There are no records in the list,
*** prohibit selection from list.

IF  lnTrLines = 0
  _CUROBJ          = OBJNUM(pbNew) 
  RETURN .F.
ENDIF

*** If current record has no account code entry,
*** prohibit selection from list.

IF VAL(STRTRAN(lcAcctCode,'-',''))=0 
  RETURN .F.
ENDIF

SELECT (lc_TmpTrDt)
*** Refresh get fields with current contents
lnOldRec   = lsTrnDet
lcTrdtexp  = &lc_TmpTrDt..cTrdtexp 
lcAcctCode = &lc_TmpTrDt..cAcctcode
lcAcctDesc = LOOKUP(GLACCHAR.cAccnldes,&lc_TmpTrDt..cAcctcode,;
                    GLACCHAR.cAcctcode,'ACCTCODE')

IF cDrOrCr ="D"
  lnDebit          = nAmount
  lnCredit         = 0 
ELSE
  lnCredit         = nAmount
  lnDebit          = 0
ENDIF

SHOW GET lcTrdtexp  
SHOW GET lcAcctCode 
SHOW GET lcAcctDesc   DISABLE
SHOW GET lnDebit    
SHOW GET lnCredit   
SELECT GLTRNSHD
 
*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
* 
*    VALID function for push button "New" (pbNew).
*

FUNCTION lfvNew

*** Are there any empty records in the file?
*** If there are,find them and replace them with the new values
*** else Insert a new record and prepare it to be filled by the user ,
*** initializing it with type ("D"),distribution code and status="A" 
*** ( for addition )

SELECT  (lc_TmpTrDt)

INSERT INTO &gcWorkDir.&lc_TmpTrDt ;
       (cBatchno,cTranno,dTrnpDate,cTrnpYr,cTrnpPrd,cStatus);
       VALUES (laData[1],laData[2],laData [3],laData [6],laData [7],'A')
*** Add Audit Information to the newly created record
=gfAdd_Info()

*** The following fields are blanked,waiting for an entry
*** When they are entered,their valid functions take care of their saving 

lcAcctcode = REPLICATE("0",lnAcsSegSz) 

*B600391,1 Blank both account desc. & the comment if add new account.
lcAcctDesc = ""
lcTrdtexp  = ""

lnDebit    = 0
lnCredit   = 0

*** Increase number of records in temporary file
lnTrLines  = lnTrLines + 1

*** Select the new record from the list
lsTrnDet   = lnTrLines

*** Refresh objects
SHOW GET lcAcctCode  ENABLE

*B600390,1 Refresh the acc. browse invisable button with every account
*B600390,1 refreshing in the different modes.
SHOW GET ibActBrow   ENABLE

lcAccSt  = 'ENABLE'    

IF _DOS
   SHOW GET ibActBrow  ENABLE
ELSE
  SHOW GET pbActBrow  ENABLE
ENDIF  
 
*** Disable numeric fields until an account is entered.
SHOW GET lcTrdtexp    DISABLE

*B600391,1 Refresh the account desc. if add new account.
SHOW GET lcAcctDesc   DISABLE

lcDrCrSt = 'DISABLE'
SHOW GET lnDebit      DISABLE
SHOW GET lnCredit     DISABLE
SHOW GET pbAdj        DISABLE
lcAdjSt = 'DISABLE'

IF lnTrLines=1
  SHOW GET pbRem      ENABLE
  lcRemSt = 'ENABLE'
  lcOthSt = 'DISABLE'
ENDIF

lctempStat = IIF(lnTrLines= 0 ,'ENABLE' ,'DISABLE')
SHOW GET laData[3]   &lctempStat
SHOW GET laData[16]  &lctempStat
SHOW GET ibTempCode  &lctempStat
lcOthSt = lctempStat

*** Disable New button until a valid account is ebtered
SHOW GET pbNew       DISABLE
lcNewSt = 'DISABLE'

*** Do not forget to refresh the list
SHOW GET lsTrnDet  

*** Prepare the user for entry by moving the cursor
*** (activating object) to the cAcctCode field (lcAcctCode object) 
_CUROBJ       = OBJNUM(lcAcctcode)

*** Always return to the original work aria
SELECT  GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvRem
*!
*!**************************************************************************
*
*    VALID function for push button "Remove" (pbRem).
*  
FUNCTION lfvRem
*** Confirm Removing of the record
IF gfModalGen("QRM00007B00007","ALERT") = 1
 
  SELECT  (lc_TmpTrDt)
  lnOldRec   = lsTrnDet
  *** If the record is previously modified,"M---->D"
  ***   delete it.
  *** If it is a new entry                 "A---->S"
  ***   skip it when saving
  *** else (a "Same" record )              "S---->D" 
  ***   delete it
  lcStatus         = SUBSTR('DDS',AT(cStatus,'SMA'),1)   
  REPLACE cStatus WITH lcStatus
  
  *** Decrement number of records in list
  lnTrLines   = lnTrLines-1
  IF lnTrLines = 0
    lcCodeType = "A" 
    laData[16] = SPACE(8)
    lcOldCode  = SPACE(8)
    lcAutDes   = SPACE(40)    
    laData[8]  = 'E'
    =lfRefresh() 
  ENDIF 
  *** Adjust totals
  IF cDrOrCr ="D"
    laData[9]    = laData[9]  - nAmount
  ELSE
    laData[10]   = laData[10] - nAmount
  ENDIF
  *** Delete the current record (to be removed )
  *** If the removed record is the last one,go top 
  DELETE
  *** Check if you have to go to next record or the top one
  SKIP 
  IF EOF(lc_TmpTrDt)
    GO TOP
  ENDIF
  lsTrnDet   = lnOldRec
  lnTotalDr  = MAX( laData[9], laData[10])
  lnTotalCr  = lnTotalDr
  lnTempBal  = ABS( laData[9] - laData[10])
  lnBalance = lnTempBal
  lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
  lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
  lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)
  lnDebit    = IIF(lnTrLines=0 .OR. cDrOrCr="C",0,nAmount)
  lnCredit   = IIF(lnTrLines=0 .OR. cDrOrCr="D",0,nAmount)
  lcTrdtexp  = IIF(lnTrLines=0,SPACE(40),&lc_TmpTrDt..cTrdtexp) 
  lcAcctCode = IIF(lnTrLines=0,REPLICATE ("0",lnAcsSegSz),cAcctCode)
  lcAcctDesc = IIF(lnTrLines=0,SPACE(60),LOOKUP(GLACCHAR.cAccnlDes,;
                        &lc_TmpTrDt..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE'))
  lcObjStat  = IIF(lnTrLines = 0,"DISABLE","ENABLE")
  lctempStat = IIF(lnTrLines= 0 ,'ENABLE' ,'DISABLE')

  SHOW GET laData[3 ]  &lctempStat
  SHOW GET laData[16]  &lctempStat
  SHOW GET ibTempCode  &lctempStat  
  lcOthSt = lctempStat
  SHOW GET lcTrdtexp  DISABLE
  SHOW GET lcAcctDesc DISABLE
  SHOW GET lnTranBalD DISABLE
  SHOW GET lnTranBalC DISABLE
  SHOW GET lnTotalDr  DISABLE
  SHOW GET lnTotalCr  DISABLE
  SHOW GET lcAcctCode &lcObjStat
  
  *B600390,1 Refresh the acc. browse invisable button with every account
  *B600390,1 refreshing in the different modes.
  SHOW GET ibActBrow  &lcObjStat
  
  lcDrCrSt = lcObjStat
  lcAccSt  = lcObjStat
  SHOW GET lnDebit    &lcObjStat
  SHOW GET lnCredit   &lcObjStat
  SHOW GET pbRem      &lcObjStat
  SHOW GET pbNew      ENABLE
  lcNewSt = 'ENABLE'
  lcRemSt = lcObjStat
  lcOthSt = IIF(lnTrLines <> 0,"DISABLE","ENABLE")

  IF _DOS
    SHOW GET ibActBrow  &lcObjStat
  ELSE
    SHOW GET pbActBrow  &lcObjStat
  ENDIF  
  lcObjStat  = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
  lcAdjSt = lcObjStat
  SHOW GET pbAdj &lcObjStat
  *** Update list contents
  SHOW GET lsTrnDet
  =lfStat()
  _CUROBJ  = OBJNUM(lsTrnDet)
ENDIF
SELECT  GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvAccCode
*!
*!**************************************************************************
*
*  Valid function for the field lcAcctcode
* 
FUNCTION lfvAccCode
PRIVATE lcCodeT

IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND. !ISDIGIT(LTRIM(lcAcctCode))
  =gfModalGen("TRM02061B00000","Dialog")
  lcAcctCode       = lcOldAcct  
  SHOW GET   lcAcctCode
  _CUROBJ          = OBJNUM(lcAcctCode)
  RETURN
ENDIF
    
IF lcAcctCode = lcOldAcct .AND. !llFromBton 
  *** No need for validation
  RETURN
ELSE
  IF llFromBton
    lcOldAcct  = lcOldAcct1
    llFromBton = .F.
  ENDIF
  *** This condition is true only if the account code had an old entry
  *** and now it is emptied,just ignore the entry.
  IF LEFT(LTRIM(lcAcctCode),1)<>'?'.AND.VAL(STRTRAN(lcAcctCode,'-','')) = 0 
    lcAcctCode     = lcOldAcct  
    SHOW GET   lcAcctCode
  ELSE
    lcCodeT        = ''
    *** If there is only one record in the temporary file,
    *** The Account Type may be changed.
    *** This condition applies for the following cases :
    *** a. A NEW entry ( Add mode, or Edit mode) in an empty list.
    *** b. Removing all records except one.
    *** c. Removing all records ( handled from NEW )
    IF lnTrLines  = 1 
      IF !llFromBat
        lcCodeType   = "A"
     ELSE
        lcCodeType  =IIF(GLBATCH.cBatType='S','S','T')
     ENDIF 
    ENDIF

    IF lfVldAccnt(lcCodeType,"C","L",.T.,@lcAcctDesc,@lcCodeT,"") .AND. !EMPTY(lcAcctCode)
      CLEAR TYPEAHEAD
      IF lcCodeType="A"
        lcCodeType = IIF(LEFT(lcCodeT,1)="Y","S","T")
        laData[19]=IIF(lcCodeType="S",'N','Y')
      ENDIF    

      SELECT (lc_TmpTrDt)

      *** If previously modified,"M---->M"
      *** If a new entry,        "A---->A"
      *** else                   "S---->M"       

      lcStatus     = SUBSTR("MAM",AT(cStatus,"MAS"),1)
      REPLACE &lc_TmpTrDt..cAcctcode WITH lcAcctCode ,;
              &lc_TmpTrDt..cStatus   WITH lcStatus

      IF EMPTY(&lc_TmpTrDt..cDrOrCr)
        REPLACE &lc_TmpTrDt..cDrOrCr   WITH "D"
      ENDIF

      *** Refresh objects
      SHOW GET lsTrnDet
      SHOW GET lcAcctDesc DISABLE

      =lfRefresh()  
      SELECT GLTRNSHD
      
      *** In this screen, both debit and credit fields may have
      *** zeroes in the same record,hence,pbNew may be enabled now..
      *** provided that, if in percent mode,totals do not exceed 100% 

      SHOW GET pbNew     ENABLE
      lcNewSt = 'ENABLE'
      lcDrCrSt= 'ENABLE'
      SHOW GET lnDebit   ENABLE
      SHOW GET lnCredit  ENABLE
      SHOW GET lcTrdtexp ENABLE

      lcObjState   = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
      lcAdjSt   = lcObjStat
      SHOW GET pbAdj     &lcObjState.
     ELSE
      lcAcctCode   = lcOldAcct
      SHOW GET  lcAcctCode   
      _CUROBJ      = OBJNUM(lcAcctCode)
    ENDIF
  ENDIF
ENDIF

SELECT GLTRNSHD


*!**************************************************************************
*!
*!      Function:  lfvDebit
*!
*!**************************************************************************
*    VALID function for get field "lnDebit".
*    laData[9] field is used for totals
*   
FUNCTION lfvDebit

Private lnOldCredit

lnOldCredit = lnCredit
           
IF lnDebit <> lnOldDr
  *** Reject negative entries
  IF lnDebit < 0
    =gfModalGen("TRM02036B00000","DIALOG")
    lnDebit  = lnOldDr  
    _CUROBJ  = OBJNUM(lnDebit)
    SHOW GET lnDebit
  ELSE 
    SELECT (lc_TmpTrDt)
    *** If the record has been previously saved as a credit,
    *** and now there is a debit entry,
    IF lnDebit > 0 .AND. cDrOrCr="C"
      *** Ignore the credit entry,blank it and adjust Credit totals. 
      lnCredit     = 0      
      laData[10]   = laData[10]-nAmount  
      laData[9]    = laData[9]+lnDebit
      SHOW GET lnCredit    
    ELSE 
      laData[9]   = laData[9]-nAmount+lnDebit
    ENDIF

    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnDebit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "D" 

     IF lnTrLines = 1 .AND. lnOldCredit=0
       =lfDist() 
     ENDIF  

    lnTotalDr  = MAX( laData[9], laData[10])
    lnTotalCr  = lnTotalDr
    lnTempBal  = ABS( laData[9] - laData[10])
    lnBalance = lnTempBal
    lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

    *** Adjust controls
    lcObjState     = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
    lcAdjSt = lcObjStat
    SHOW GET pbAdj     &lcObjState
    =lfRefresh()
  ENDIF
  lcObjState         = IIF(VAL(STRTRAN(lcAcctCode,'-',''))>0;
                      .AND.(lnDebit>0 .OR. lnCredit>0),"ENABLE","DISABLE")

  lsTrnDet   = RECNO()
  SHOW GET lsTrnDet
  SHOW GET pbNew      &lcNewSt
  SHOW GET lnTranBalD DISABLE
  SHOW GET lnTranBalC DISABLE
  SHOW GET lnTotalDr  DISABLE
  SHOW GET lnTotalCr  DISABLE
 
  =lfStat()
  
  *B500782,1 Reresh the total debit & credit & balance in the transction screen.
  SHOW GET laData[9]
  SHOW GET laData[10]
  SHOW GET lnBalance
ENDIF
lnOldCredit=0
SELECT GLTRNSHD
 
*!**************************************************************************
*!
*!      Function:  lfvCredit
*!
*!
*!**************************************************************************
*    VALID function for get field "lnCredit".
*    laData[10] field is used for totals
*   
FUNCTION lfvCredit

Private lnOldDebit
lnOldDebit=lndebit

IF lnCredit <> lnOldCr     
  
  *** Reject negative entries
  IF lnCredit < 0
    =gfModalGen("TRM02036B00000","DIALOG") 
    lnCredit       = lnOldCr  
    SHOW GET lnCredit
    _CUROBJ        = OBJNUM(lnCredit)
  ELSE 

    SELECT (lc_TmpTrDt)
    IF lnCredit > 0 .AND. cDrOrCr="D"
        *** If there is a Debit entry for the same record,ignore it,
        *** blank it and adjust Debit totals. 
        lnDebit    = 0
        laData[9]    = laData[9]-nAmount  
        laData[10]    = laData[10]+lnCredit
        SHOW GET lnDebit
    ELSE
        laData[10]    = laData[10]-nAmount+lnCredit
    ENDIF

    lcStatus   = SUBSTR("MAM",AT(cStatus,"MAS"),1)
    REPLACE nAmount  WITH lnCredit ;
            cStatus  WITH lcStatus;
            cDrOrCr  WITH "C" 
    
    IF lnTrLines = 1 .AND. lnOldDebit=0
      = lfDist()
    ENDIF  

    lnTotalDr  = MAX( laData[9], laData[10])
    lnTotalCr  = lnTotalDr
    lnTempBal  = ABS( laData[9] - laData[10])
    lnBalance = lnTempBal
    lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
    lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
    lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

    *** Adjust controls
    SHOW GET lsRcrDet
    lcObjState     = IIF(laData[9]<>laData[10],"ENABLE","DISABLE")
    lcAdjSt = lcObjStat
    SHOW GET pbAdj     &lcObjState.
    =lfRefresh()

  ENDIF
  lcObjState         = IIF(VAL(STRTRAN(lcAcctCode,'-',''))>0;
                     .AND.(lnDebit>0 .OR. lnCredit>0),"ENABLE","DISABLE")

  lsTrnDet   = RECNO()
  SHOW GET pbNew      &lcNewSt
  SHOW GET lsTrnDet
  SHOW GET lnTranBalD DISABLE
  SHOW GET lnTranBalC DISABLE
  SHOW GET lnTotalDr  DISABLE
  SHOW GET lnTotalCr  DISABLE
 
  =lfStat()
  
  *B500782,1 Reresh the total debit & credit & balance in the transction screen.
  SHOW GET laData[9]
  SHOW GET laData[10]
  SHOW GET lnBalance
ENDIF
lnOldDebit=0
SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvTrdtexp
*!
*!**************************************************************************
*
FUNCTION lfvTrdtexp

REPLACE &lc_TmpTrDt..cTrdtexp WITH lcTrdtexp ;
        &lc_TmpTrDt..cStatus  WITH SUBSTR('MMA',AT(&lc_TmpTrDt..cStatus,'SMA'),1)   

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      Function: lfvAdjust
*!
*!**************************************************************************
*
*    VALID function for push button "Adjust" (pbAdj).
*  

FUNCTION lfvAdjust
SELECT (lc_TmpTrDt)

laData[9]  = laData[9]  - lnDebit 
laData[10] = laData[10] - lnCredit

lnDefr     = laData[9] - laData[10]
lnSign     = SIGN(lnDefr)
lnDefr     = ABS (lnDefr)

*** If the difference is positive,i.e. Debit is greater,

IF lnSign = 1
  *** Add difference to Credit field
  laData[10]   = laData[10] + lnDefr
  lnCredit     = lnDefr
  lnDebit      = 0 
ELSE
  *** If the difference is negative,i.e.Credit is greater,
  IF lnSign = -1
    laData[9]   = laData[9] + lnDefr 
    lnDebit     = lnDefr
    lnCredit    = 0
  *** If the difference is zero,
  ELSE
    lnDebit     = 0
    lnCredit    = 0
  ENDIF  
ENDIF

lnTotalDr  = MAX( laData[9], laData[10])
lnTotalCr  = lnTotalDr
lnTempBal  = ABS( laData[9] - laData[10])
lnBalance = lnTempBal
lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

REPLACE  cDrOrCr WITH IIF(lnDebit>0,"D",IIF(lnCredit>0,"C"," "));
         nAmount WITH lnDefr ;
         cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)    

SHOW GET lsTrnDet
SHOW GET lnDebit
SHOW GET lnCredit
SHOW GET pbAdj      DISABLE
lcAdjSt = 'DISABLE'
SHOW GET lnTranBalD DISABLE
SHOW GET lnTranBalC DISABLE
SHOW GET lnTotalDr  DISABLE
SHOW GET lnTotalCr  DISABLE

=lfStat()
*B801123,1 AMM 07/29/97. start
SHOW GET laData[9]
SHOW GET laData[10]
SHOW GET lnBalance
*B801123,1 AMM 07/29/97. end

_CUROBJ     = OBJNUM(pbDtClose)

SELECT GLTRNSHD

IF VAL(STRTRAN(lcAcctCode,'-',''))>0
  SHOW GET pbNew ENABLE
  lcNewSt = 'ENABLE'
ENDIF  

SELECT GLTRNSHD
  
*!**************************************************************************
*!
*!      Function: lfTrnDet
*!
*!**************************************************************************
*   VALID function for push button "Adjust" (pbAdj).
*  
FUNCTION lfTrnDet

DO CASE
 CASE _DOS
    RETURN IIF(&lc_TmpTrDt..cDrorcr = 'D',STR(&lc_TmpTrDt..nAmount,15,2)+'³'+;
           SPACE(15),SPACE(15)+'³'+STR(&lc_TmpTrDt..nAmount,15,2))
  CASE _WINDOWS
     RETURN IIF(&lc_TmpTrDt..cDrorcr = 'D',STR(&lc_TmpTrDt..nAmount,15,2),+;
                SPACE(15)+STR(&lc_TmpTrDt..nAmount,15,2))
ENDCASE  

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
* Save transaction header and details and update total 
* batch debit and credit in case comming from batch program
*
PROCEDURE lpSavScr
PRIVATE lcOrder,lcRecNo,lnOldCr,lnOldDr

*E300692,1 Change file name from SYCFSPRD to FSPRD
*SELECT SYCFSPRD
SELECT FSPRD
*E300692,1 end
SET ORDER TO COMFYRPRDI

*SEEK (gcAct_Comp+laData[6]+laData[7])
SEEK (gcPrnt_Cmp+laData[6]+laData[7])

IF lfsplocks
  SELECT GLTRNSHD
  =gfModalGen("TRM02210B00000","Dialog",laData[7]+'-'+laData[6])
  llcSave=.F.
  _CUROBJ=OBJNUM(laData[3])
  RETURN
ENDIF

SELECT GLTRNSHD

DO CASE
  *** User have to add lines with amounts > 0 
  CASE laData[9] = 0 .AND. laData[10] = 0
    *** Can`t save an empty transaction
    =gfModalGen("TRM02012B00000","Dialog")
    llCSave  = .F.
    RETURN
  CASE VAL(STRTRAN(lcAcctCode,'-',''))=0
    =gfModalGen("TRM02022B00000","DIALOG")  
    llCSave        = .F.
    _CUROBJ        = OBJNUM(lcAcctCode)
    RETURN

  OTHERWISE
    *** If total debit = total credit transaction will be balanced;
    *** else out of balance
    IF laData[9]=laData[10]
      laData[8]=IIF(cbHold=0,'U','H')
    ELSE
      laData[8]='O'
      cbhold   = 0
    ENDIF

    SELECT GLTRNSHD
    IF laScrMode[4]

      *** Creating transaction  sequence

      *E300663,1 Change this line for the changes we have 
      *          made to (gfSequence) [Begin]
      *laData[2]  = RIGHT('00000000'+ALLTRIM(STR(gfSequence('TRANSACT',1))),8)
      laData[2]  = gfSequence('CTRANNO')
      *E300663,1 Change this line [End]
      
      *** Update the lines with batch and transaction no. 

      SELECT GLTRNSHD
      *** Save header to master
      APPEND BLANK
      GATHER FROM laData FIELDS &lcScFields                              
      =gfAdd_Info('GLTRNSHD')

      SELECT (lc_TmpTrDt)      
      REPLACE ALL cbatchno  WITH laData[1],;
                  cTranno   WITH laData[2],;
                  cAdd_User WITH gcUser_ID,;
                  dAdd_Date WITH DATE(),;
                  cAdd_Time WITH gfGetTime()

      *** Save lines to master
      =gfTmp2Mast('GLTRNSDT',lc_TmpTrDt) 

      *** Update total batch debit and credit if comming from batch program 
      IF llFromBat

        SELECT glTrnshd
        lcRecNo=RECNO()
        lcOrder=SYS(22)
        SET ORDER TO TRANSTAT

        IF !SEEK('O'+lcBatchno) 
          SELECT glBatch  
          REPLACE nBaTotDr WITH nBaTotDr+laData[9];
                  nBaTotCr WITH nBaTotCr+laData[10];
                  cBatStat WITH 'U'
          ELSE
          SELECT glBatch  
          REPLACE nBaTotDr WITH nBaTotDr+laData[9];
                  nBaTotCr WITH nBaTotCr+laData[10];
                  cBatStat WITH 'O'
        ENDIF
 
        SELECT glTrnshd
        SET ORDER TO lcORDER
        GOTO lcRecNo
      ENDIF

      *** Transaction saved with new seq. no ---  
      =gfModalGen("TRM02014B00000","DIALOG",laData[2])
     
      IF WVISIBLE('CWRGLTRDT')
        =gfChClose('CWRGLTRDT')
      ENDIF  

    ELSE

        SELECT GLTRNSHD
        *** Save header record to master
        lnOldCr=gltrnshd.nTrntotCr
        lnOldDr=gltrnshd.nTrntotDr
        
        GATHER FROM laData FIELDS &lcScFields 
        =gfAdd_Info('GLTRNSHD')

        *** Save lines to master
        SELECT (lc_TmpTrDt)      
        =gfTmp2Mast('GLTRNSDT',lc_TmpTrDt) 

       *** Update total batch debit and credit if comming from batch program   
       IF llFromBat
         SELECT glTrnshd
         lcRecNo=RECNO()
         lcOrder=SYS(22)
         SET ORDER TO TRANSTAT
         IF !SEEK('O'+lcBatchno) 
           SELECT glBatch  
           REPLACE nBaTotDr WITH nBaTotDr-lnOldDr+laData[9];
                   nBaTotCr WITH nBaTotCr-lnOldCr+laData[10];
                   cBatStat WITH 'U'
           ELSE
           SELECT glBatch  
           REPLACE nBaTotDr WITH nBaTotDr-lnOldDr+laData[9];
                   nBaTotCr WITH nBaTotCr-lnOldCr+laData[10];
                   cBatStat WITH 'O'
         ENDIF
         SELECT glTrnshd
         SET ORDER TO lcORDER
         GOTO lcRecNo
        ENDIF
      ENDIF

ENDCASE
SELECT GLTRNSHD  

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*
* No deletion just void this transaction
*
PROCEDURE lpDelScr
PRIVATE lcOrder,lcRecNo

IF lfCheckRec()
  laScrMode = .F.
  laScrMode [1] = .T.  
  REPLACE GLTRNSHD.cTrnStat WITH 'V'
 =gfAdd_Info('GLTRNSHD')
 =gfObj_Lock(.F.) 
 
 IF llFromBat
    SELECT glTrnshd
    lcRecNo=RECNO()
    lcOrder=SYS(22)
    SET ORDER TO TRANSTAT
    IF !SEEK('O'+lcBatchno) 
      SELECT glBatch  
      REPLACE nBaTotDr WITH nBaTotDr-laData[9];
              nBaTotCr WITH nBaTotCr-laData[10];
             cBatStat WITH IIF(nBaTotDr=0,'E','U')
    ELSE
       SELECT glBatch  
       REPLACE nBaTotDr WITH nBaTotDr-laData[9];
               nBaTotCr WITH nBaTotCr-laData[10];
              cBatStat WITH 'O'
    ENDIF

    SELECT glTrnshd
    SET ORDER TO lcORDER
    GOTO lcRecNo
   ENDIF

 IF WVISIBLE('CWRGLTRDT')
   =gfChClose('CWRGLTRDT')
 ENDIF  

 SELECT GLTRNSHD

ENDIF

SELECT GLTRNSHD


*!**************************************************************************
*!
*!      FUNCTION: lfvTemp
*!
*!**************************************************************************
* This function will be called if adding new transaction and 
* user select a template code to generat from
* teplate could be based on amounts or percentage
*
FUNCTION lfvTemp
*** Get the ammoubt to be distributed accourding to percentages

IF lcAutBase='P'
  *E300683,5 Call *.SPR from screens directory
  * DO GLAUTDS.SPR 
  DO (gcScrDir + gcWinAppl + '\GLAUTDS.SPR')
  *E300683,5 end  
  IF lnAmount <= 0  
    RETURN .F.
  ENDIF
ENDIF  

*** Creat transaction lines from automatic detail file
SELECT  laData[1] AS 'cBatchno',SPACE(8) AS 'cTranno',;
        GLAUTDT.cAcctCode,SPACE(40) AS 'cTrdtexp',GLAUTDT.cDrOrCr, ;
        GLAUTDT.nAmount,laData[3] AS 'dTrnpDate',laData[6] AS 'ctrnpyr',;
        laData[7] AS 'ctrnpprd',;
        GLAUTDT.cAdd_User,GLAUTDT.dAdd_Date,GLAUTDT.cAdd_time,;
        00000 AS 'nRecNo' , "A" AS 'cStatus';
   FROM &gcDataDir.GLAUTDT;
   INTO DBF &gcWorkDir.&lc_TmpTrDt;
  WHERE cAutType+cAutCode='T'+laData[16] 


lnTrLines  = _TALLY
lsTrnDet   = 1    
lcAcctCode = &lc_TmpTrDt..cAcctcode   
lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
laData[19]=IIF(lcCodeType="S",'N','Y')

IF lcAutBase='P'
  REPLACE ALL &lc_TmpTrDt..nAmount WITH ;
               ( &lc_TmpTrDt..NAmount * lnAmount ) / 100
ENDIF

SELECT (lc_TmpTrDt)

GO TOP 

*** Calculate total debit and total credit
SUM IIF(CDrOrCr="D",nAmount,0),IIF(cDrOrCr="C",nAmount,0) ;
    TO laData[9],laData[10]


lnTotalDr  = MAX( laData[9], laData[10])
lnTotalCr  = lnTotalDr
lnTempBal  = ABS( laData[9] - laData[10])
lnBalance  = lnTempBal
lcDrOrCr   = IIF(lnBalance=0,' ',IIF(laData[9] > laData[10],'Cr','Dr'))
lnTranBalC = IIF( laData[9] > laData[10] , lnTempBal, 0)
lnTranBalD = IIF( laData[9] < laData[10] , lnTempBal, 0)

SHOW GET lsTrnDet  
SHOW GET lnTranBalD  DISABLE
SHOW GET lnTranBalC  DISABLE
SHOW GET lnTotalDr   DISABLE
SHOW GET lnTotalCr   DISABLE

IF lnTrLines > 0

  SHOW GET pbRem      ENABLE
  lcRemSt = 'ENABLE'
  lcOthSt = 'DISABLE'
  lcDrCrSt = 'ENABLE'
  lcAccSt  = 'ENABLE'    
  SHOW GET lcAcctCode ENABLE
  
  *B600390,1 Refresh the acc. browse invisable button with every account
  *B600390,1 refreshing in the different modes.
  SHOW GET ibActBrow  ENABLE
  
  SHOW GET lnDebit    ENABLE
  SHOW GET lnCredit   ENABLE
  SHOW GET lcTrdtexp  ENABLE
  IF _DOS
    SHOW GET ibActBrow  ENABLE
  ELSE
    SHOW GET pbActBrow  ENABLE
  ENDIF  

  IF laData[9] <> laData[10]
    SHOW GET pbAdj    ENABLE
    lcAdjSt = 'ENABLE'
  ENDIF  

ENDIF

=lfRefresh()
*** Disable the template code field if the code is used to creat lines
SHOW GET laData[3 ]  DISABLE
SHOW GET laData[16]  DISABLE
SHOW GET ibTempCode  DISABLE
lcOthSt = 'DISABLE'

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION: lfDist
*!
*!**************************************************************************
*
FUNCTION lfDist

PRIVATE llDrorCr,lcOldTag,lc_cSegAutDs
SELECT (lc_TmpTrDt)
lcDstCode = LOOKUP(GLACCHAR.cSegAutDs,&lc_TmpTrDt..cAcctCode,GLACCHAR.cAcctCode,'ACCTCODE')
IF !EMPTY(lcDstCode)

  IF  gfModalGen('QRM02166B00006','DIALOG')=1

    llValidtmp = .T.
    SELECT  GLAUTDT
    SET ORDER TO TAG TYPCODACC IN GLAUTDT

    IF SEEK('D'+lcDstCode)

    SET ORDER TO TAG ACCTCODE  IN GLACCHAR
    SET RELATION TO glautdt.cacctcode INTO GLACCHAR ADDITIVE
    SCAN REST WHILE cauttype + cautcode = 'D'+lcDstCode
      DO CASE
        CASE GLACCHAR.cSegActiv='I'
          *** This templet has one or more inactiv account 
          =gfModalGen("TRM02164B00000",'DIALOG','distribution')
          llValidtmp = .F.
          EXIT
        CASE GLACCHAR.cSegAlPos='N'
          *** This templet has one or more account(s) not allowed for posting
          =gfModalGen("TRM02163B00000",'DIALOG','distribution')
          llValidtmp = .F.
          EXIT
        CASE GLACCHAR.cStandard <> laData[19]
          IF laData[19]='N'  
            =gfModalGen("TRM02030B00000",'DIALOG',GLACCHAR.cAcctCode )
          ELSE
            =gfModalGen("TRM02031B00000",'DIALOG',GLACCHAR.cAcctCode )
          ENDIF
          llValidtmp = .F.
          EXIT
      ENDCASE  
    ENDSCAN

    SELECT  GLAUTDT
    SET RELATION TO

    IF llValidtmp 
      SELECT (lc_TmpTrDt)
      SCATTER MEMVAR 
      lnActAmnt = nAmount
      llDrorCr  = (cDrorCr="D")
      *** Creat transaction lines from automatic detail file
      ***
      SELECT  laData[1] AS 'cBatchno',SPACE(8) AS 'cTranno',;
              GLAUTDT.cAcctCode,SPACE(40) AS 'cTrdtexp',GLAUTDT.cDrOrCr, ;
              GLAUTDT.nAmount,laData[3] AS 'dTrnpDate',laData[6] AS 'ctrnpyr',;
              laData[7] AS 'ctrnpprd',;
              GLAUTDT.cAdd_User,GLAUTDT.dAdd_Date,GLAUTDT.cAdd_time,;
              00000 AS 'nRecNo' , "A" AS 'cStatus';
         FROM &gcDataDir.GLAUTDT;
         INTO DBF &gcWorkDir.&lc_TmpTrDt;
        WHERE cAutType+cAutCode='D'+lcDstCode
      lnTrLines  = _TALLY+1
      lsTrnDet   = 1    
      SELECT (lc_TmpTrDt)
      REPLACE ALL nAmount WITH (nAmount * lnActAmnt) / 100;
                  cDrorCr WITH IIF(llDrorCr,'C','D')
      IF llDrorCr
        SUM nAmount TO laData[10]
      ELSE
        SUM nAmount TO laData[9]
      ENDIF  
      APPEND BLANK
      GATHER MEMVAR
      lcCodeType = IIF(LEFT(LOOKUP(GLACCHAR.cTypeCode,&lc_TmpTrDt..cAcctcode,;
                     GLACCHAR.cAcctCode,'ACCTCODE'),1)="Y","S","T")
      laData[19]=IIF(lcCodeType="S",'N','Y')
     ENDIF 
    ELSE
      =gfModalGen('TRM02214B00000','ALERT',lcDstCode)
    ENDIF 
  ENDIF
ENDIF

SELECT GLTRNSHD

*!**************************************************************************
*!
*!      FUNCTION: lfStat
*!
*!**************************************************************************
*

FUNCTION lfStat

PRIVATE lcOldStat,lnOldHold,lcHoldStat
lcOldStat=laData[8]
lnOldHold=cbHold

IF laData[9]<>laData[10]
  laData[8]='O'  
ELSE
  laData[8]=IIF(ladata[9]<>0,'U','E')
ENDIF

lcHoldStat=IIF(laData[8]='U','ENABLE','DISABLE')

=lfRefresh()
cbHold=IIF(lcOldStat=laData[8],lnOldHold,0)
SHOW GET cbHold     &lcHoldStat


*!**************************************************************************
*!
*!      FUNCTION: lfControl
*!
*!**************************************************************************
*

FUNCTION lfControl

lcNewMode=IIF(!laScrMode[2] ,'ENABLE','DISABLE')
SHOW GET ibBgDumi2   ENABLE
SHOW GET ibEndDumi2  ENABLE
SHOW GET pbNew       &lcNewMode
*B600499,1 Change the mode of the Remove entries button with the screen status
SHOW GET pbRem       &lcNewMode
lcNewSt = lcNewMode
*B600499,1 Save the Remave button Status
lcRemSt = lcNewMode
SHOW GET pbDtClose   ENABLE

lcSavError=ON("ERROR")
ON ERROR X=1
SELECT (lc_TmpTrDt)
SHOW GET lsTrnDet   ENABLE
ON ERROR &lcSavError
SELECT GLTRNSHD


*!**************************************************************************
*!
*!      FUNCTION: lfCheckRec
*!
*!**************************************************************************
*

PROCEDURE lfCheckRec

lcLoclShow = "lpShow"
SHOW GET pbDlt,1 &lcPrompt &lcObjStatus
*B600486,1 Control the status of the delete bar in the record pad.
laCtrStat[8] = lcObjStatus

IF ! laScrMode[2] 
  RETURN
ENDIF  

IF EMPTY(laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

IF lcBatchno+laData[2] <> glTrnsHd.cBatchNo+glTrnsHd.cTranno
  =gfModalGen("TRM02187B00000","DIALOG",;
              "Transaction"+'|'+"voided")
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  SHOW GETS   
  RETURN .F.
ENDIF


IF ( lcBStamp = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time ) ;
   .AND. ( laData[8] = glTrnsHd.cTrnStat )
  RETURN .T.
ENDIF

lcAdd_User =ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glTrnsHd.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time =glTrnsHd.cAdd_Time
lcAdd_Date =dtoc(glTrnsHd.dAdd_Date)
lcPost_User=ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glTrnsHd.cPostUser,SYUUSER.cUser_Id))
lcPost_Time=glTrnsHd.cPostTime
lcPost_Date=dtoc(glTrnsHd.dPostDate)


IF glTrnsHd.cTrnStat = 'V' 
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"voided"+'|'+;
     lcAdd_User+'|'+lcAdd_Date +' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  =gfObj_Lock(.F.) 
  SHOW GETS   
  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat = 'A'
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"approved"+'|'+;
     lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
  laScrMode = .F.
  laScrMode [3] = .T.     
  =gfObj_Lock(.F.) 
  SHOW GETS 
  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat = 'P'
  =gfModalGen("TRM02167B00000","DIALOG",;
     "Transaction"+'|'+"posted"+'|'+;
     lcPost_User+'|'+lcPost_Date+' '+lcPost_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
  laScrMode = .F.
  laScrMode [3] = .T.   
  =gfObj_Lock(.F.) 
  SHOW GETS 
  RETURN .F.
ENDIF

IF glTrnsHd.cTrnStat $ 'HOEU'
  =gfModalGen("TRM02181B00000","DIALOG",;
  "Transaction"+'|'+;
   lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time+'|'+"Transaction")
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glTrnsHd.dAdd_Date) + glTrnsHd.cAdd_Time 
  =gfObj_Lock(.F.) 
  SHOW GETS 
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: gfCpEdit
*!
*!**************************************************************************
*

FUNCTION gfCpEdit

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  


IF glTrnsHd.cTrnStat $ 'APV'
  SHOW GETS
ELSE
  *B607000,1 WAB (Start)
  *DO gfCpEdit IN ARIA27
  DO gfCpEdit IN AAS_WIN.EXE
  *B607000,1 WAB (END)
ENDIF
