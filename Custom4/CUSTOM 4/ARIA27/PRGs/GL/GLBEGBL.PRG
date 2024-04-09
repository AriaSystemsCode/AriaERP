*:************************************************************************
*:
*: Procedure file: GLBEGBL.PRG
*:
*:         System: GL System
*:         Author: Malak Hanna 
*:      Copyright (c) 
*:  Last modified: 3/27/94
*:
*:  Procs & Fncts:
*:
*:    PROCEDURE lpShow       ***                                     ***
*:    FUNCTION  lfvData_1    ***                                     ***
*:    FUNCTION  lfvData_7    ***                                     ***
*:    FUNCTION  lfvPrint     ***                                     ***
*:    FUNCTION  lfvPost      ***                                     ***
*:    FUNCTION  lfvHold      ***                                     ***
*:    FUNCTION  lfvComment   ***                                     ***
*:    FUNCTION  lfwDebit     ***                                     ***
*:    FUNCTION  lfvDebit     ***                                     ***
*:    FUNCTION  lfwCredit    ***                                     ***
*:    FUNCTION  lfvCredit    ***                                     ***
*:    FUNCTION  lfwBegDet    ***                                     ***
*:    FUNCTION  lfvDeClose   ***                                     ***
*:    FUNCTION  lfSwapM2T    ***                                     ***
*:    FUNCTION  lfSwapT2M    ***                                     ***
*:    FUNCTION  lfwSelType   ***                                     ***
*:    FUNCTION  lfvSelType   ***                                     ***
*:    FUNCTION  lfvNew       ***                                     ***
*:    FUNCTION  lfvEntries   ***                                     ***
*:    FUNCTION  lfwSourJor   ***                                     ***
*:    FUNCTION  lfvSourJor   ***                                     ***
*:    FUNCTION  lfvInclEmp   ***                                     ***
*:    PROCEDURE lpDelScr     ***                                     ***
*:    FUNCTION  lfValidAcct  ***                                     ***
*:    FUNCTION  lfTherm      ***                                     ***
*:    PROCEDURE lpSavScr     ***                                     ***
*:    PROCEDURE lfCheckRec   ***                                     ***
*:    FUNCTION  gfCpEdit     ***                                     ***
*:               
*:      Documented 1/26/1994
*:************************************************************************
*B600486,1 Reham On 06/21/95
*B600486,1 Control the array elemets that hold the control pannel button
*B600486,1 status for <Delete> button "laCtrStat[8]" & for <Edit> button
*B600486,1 "laCtrStat[7]" to control the the edit & delete Bars in the
*B600486,1 record pad.
*B601099,1 M.H 06/27/96 Hide push button print.
*E300663,1  HS 04/23/97 Change the calling of the function [gfSequence]
*E300663,1              for the changes we have made to that function.
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*E301077,71 WALID 03/07/99 Open GLBATCH through gfSetup()
*B607000,1 WAB 03/03/2003  Fix the bug of Cance.bmp not found
*:************************************************************************
*

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laCtrStat


** laKeyField[1,4]  :  Holds the key filed.
** laTstatus [7,1]  :  Holds all batches status.
** laFisYear [3,1]  :  Holds the posting years window.
** laAccTypes[1,1]  :  Holds the account types.
** laSourcJor[1,1]  :  Holds the source journals.
** laWndObj  [3,3]  :  Holds first and last objects in all screens
** laFileStru[1]    :  Holds the file structure (GLTRNSDT)

DECLARE laKeyField[1,4] , laTstatus[7,1] , laFisYear[3,1] , laAccTypes[1,1] ,;
        laSourcJor[1,1] , laWndObj[3,3]  , laFileStru[1]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'BATCHNO'
laKeyField[1,4] = 1

lcDelMesag     = "void"   && holds the prompt name insted of delete.
lc_Title       =  "Beginning batch details" && holds chield window titel.
lcPrompt       = IIF(_DOS,"PROMPT '\<Void'",'')

** lcPostPrd      : Holds the posting period + year (00-0000)
** lc_TemDet      : Holds the temporary file name(swap file).
** lc_MasDet      : Holds the temporary file name(master file).
** lcTranNo       : Holds transation number in the chield window.
** lcTranDes      : Holds transation description in the chield window.
** lcTranRef      : Holds transation reference in the chield window.
** lcTrdtexp      : Holds comment line for every account in the chield window.
** lcSourcJor     : Holds the source journal name.
** lcBegDet       : Holds the browse string (header).
** lcAcctCode     : Holds the account code in the chield window.
** lcAccDesc      : Holds the account description in the chield window.
** lcTypeId       : Holds the type ID ( CR , DR )
** lcSelType      : Holds the name of selected type.
** lcActType      : Holds the selected type code.
** lcStatus       : Holds the batch status.
** lcBalncid      : Holds the balance id ( CR , DR )
** lcScope        : Holds the filter string.
** lcBStamp       : Holds the audit information for the current batch.
** laAccTypes     : Initialize the types array.
** laFisYear      : Initialize the posting window array.
** laTstatus      : Initialize the status expressions array.
** lcYear         :
** lcObjStatus    :

** lsBegDet       : Holds the list line no.
** cbInclEmty     : Holds 1 --> include empty accounts.  
** lnInclEmty     :
** puActtype      : Holds the selected type (case windows)
** ibActtype      : Invisable but. to activate popup (case dos)
** puSourceJor    : Source journal popup.

** llglobShow     :
** llBrowse       :
** laDefProc [7]  : Use local Delete procedure
** laDefProc [9]  : Use local Save   procedure

** lnDebit        : Holds the total debits. 
** lnCredit       : Holds the total credit.
** lnSelected     : Holds the record counter for the thermometer
** lnTotalRec     : Holds the total number of records used for the thermometer       
** lncbHold       : Holds the hold status
** lnBalance      : Holds the diff. between total CR and total DR
** lnOldCred      : Holds the old credit amounts.
** lnOldDebi      : Holds the old debit  amounts.


STORE " "   TO lcPostPrd  , lc_TemDet  , lc_MasDet   , lcTranNo   ,;
               lcTranDes  , lcTranRef  , lcTrdtexp   , lcSourcJor ,;
               lcBegDet   , lcAcctCode , lcAccDesc   , lcTypeId   ,;
               lcSelType  , lcActType  , lcStatus    , lcBalncid  ,;
               lcScope    , lcBStamp   , laAccTypes  , laFisYear  ,;
               laTstatus

STORE ''    TO lcYear     , lcObjStatus

STORE 1     TO lsBegDet   , cbInclEmty , lnInclEmty  , puActtype  ,;
               ibActtype  , puSourceJor

STORE .F.   TO llglobShow , llBrowse   , laDefProc[7], laDefProc[9]

STORE 0     TO lnDebit    , lnCredit   , lnSelected  , lnTotalRec ,;
               lncbHold

STORE 0.00  TO lnBalance  , lnOldCred  , lnOldDebi

*E301077,71 [BEGIN] gfSetup() return GLBATCH selected because it is the master file
*SELECT GLBATCH
*E301077,71 [END]

IF !gfSetup()    
  RETURN
ENDIF  

llDoLocal  = .T.
lcLoclShow = "lfCheckRec"

laFisYear[1,1]  = STR(lnCurr_yer-1,4) 
laFisYear[2,1]  = STR(lnCurr_yer  ,4) 
laFisYear[3,1]  = STR(lnCurr_yer+1,4) 

laWndObj [1,1] = gcBaseWind
laWndObj [1,2] = "IBBROWKEY"
laWndObj [1,3] =  "PBPOST"

laWndObj [2,1] = "GWCCONTRL1"
laWndObj [2,2] = "PBTOP"
laWndObj [2,3] = "PBCLS"

laWndObj [3,1] = "CWRGLBTRDT"
laWndObj [3,2] = "CBINCLEMTY"
laWndObj [3,3] = "PBDTCLOSE"

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

*** check if creation of beginning balance batches is allowed.
IF ! glSetup.lSetAlBBE
  *** Creating beginning balance batches is not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02204B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

*** check if the beginning balance date is out of the posting window. 
IF ASCAN(laFisYear,STR(YEAR(glSetup.dSetBBDat),4)) = 0
  *** The beginning balance date is out of the posting  window. ***
  *** You cannot create any beginning balance batches any more. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02203B00000","DIALOG")
  glQuitting = .T.
  RETURN
ENDIF

lcWinTitl = "Beginning balance batches"   && main window titel.

IF !WEXIST(gcBaseWind)
  _TALLY = 0
  *** select all source journal from GLSUBJOR into array laSourcJor
  SELECT  cSrcJrnl+"-"+cJorlnDes;
          FROM &gcDataDir.GLSUBJOR;
          INTO ARRAY laSourcJor

  IF _TALLY = 0
    laSourcJor      = " "
    laSourcJor[1,1] = lcSJ_Def
  ELSE
    *** check if the default source journal is found ***
    *** in the array  laSourcJor.  if not found then ***
    *** inserts  the default  source journal into an ***
    *** array...
    IF ASCAN(laSourcJor,lcSJ_Def) = 0
      DIMENSION laSourcJor[ALEN(laSourcJor,1)+1,1]
      =AINS(laSourcJor,1)
      laSourcJor[1,1] = lcSJ_Def
    ENDIF  
  ENDIF
  
  lc_TemDet     = gfTempName()  
  lc_MasDet     = gfTempName() 
  
  *** create temparary file structure. ***
  CREATE DBF (gcWorkDir+lc_TemDet);
             (cAcctCode C(24)   ,;
              cAccNlDes C(65)   ,;
              nAmount   N(15,2) ,;
              nAcBopBal N(18,2) ,;
              cTrdtexp  C(40)   ,;
              cDrOrCr   C(1)  ) 

  USE &gcWorkDir.&lc_TemDet EXCLUSIVE
  SELECT (lc_TemDet)
  INDEX ON  cAcctCode TAG cAccTem
  SET ORDER TO TAG cAccTem

  SELECT GLTRNSDT
  *** create temparary file structure. ***
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'NRECNO'
  laFileStru[lnFileStru+1,2] = 'N'
  laFileStru[lnFileStru+1,3] = 10
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CSTATUS'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0

  CREATE TABLE &gcWorkDir.&lc_MasDet FROM ARRAY laFileStru 
  INDEX ON  cAcctCode TAG cAccMas
  SET ORDER TO TAG cAccMas

  DO CASE
    CASE _DOS
      lcBegDet = "SUBSTR(&lc_TemDet..cAcctCode,1,lnAcsSegSz)+' '+" +;
                 "SUBSTR(&lc_TemDet..cAccnlDes,1,30-lnAcsSegSz)+' '+"+;
                 "' '+IIF(&lc_TemDet..CDRORCR='C','(',' ')+"+;
                 "STR(&lc_TemDet..nAmount,15,2)+"+;
                 "' '+IIF(&lc_TemDet..CDRORCR='C',')',' ')+"+;
                 "' '+IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,'(',' ')"+;
                 "+STR(ABS(&lc_TemDet..nAcBopBal),18,2)+"+;
                 "IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,')',' ')"   
    CASE _WINDOWS
      lcBegDet = "SUBSTR(&lc_TemDet..cAcctCode,1,lnAcsSegSz)+' '+"+;
                 "SUBSTR(&lc_TemDet..cAccnlDes,1,23-lnAcsSegSz)+"+;
                 "' '+IIF(&lc_TemDet..CDRORCR='C','(',' ')+"+;
                 "STR(&lc_TemDet..nAmount,15,2)+"+;
                 "IIF(&lc_TemDet..CDRORCR='C',')',' ')+' '+"+;
                 "' '+IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,'(',' ')"+;
                 "+STR(ABS(&lc_TemDet..nAcBopBal),18,2)+"+;
                 "IIF(SIGN(&lc_TemDet..nAcBopBal)=-1,')',' ')"
  ENDCASE
  SELECT GLBATCH
  
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ELSE
  IF laScrMode[2] .OR. laScrMode[3]
    *** in case of return after activate the any other program. ***
    cbHold = IIF(lncbHold = 1,1,0)
    lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
    lncbHold = cbHold 
  ENDIF

ENDIF

SELECT GLBATCH

*** set filter expression to cBatType = 'B' ---> 'beginning batch'
***                       and  cBatStat $ 'EOUHAP'
*** 'empty','out of balanec','hold','approved','posted'

lcScope    = "cBatType = 'B'  .AND. cBatStat $ 'EOUHAP'"

SET FILTER TO
SET FILTER TO &lcScope

****
lcNewStat   = IIF(laScrMode[1] .OR. laScrMode[2],"ENABLE","DISABLE")
lcEntStat   = IIF(laScrMode[1],"DISABLE","ENABLE")
lcPrnStat   = IIF(laScrMode[2] .AND. laData[2]<> 'E',"ENABLE","DISABLE")
lcPosStat   = IIF(laScrMode[2] .AND. laData[2]=  'U',"ENABLE","DISABLE")
lcHldStat   = IIF(laScrMode[3] .AND. laData[2] $ 'UH',"ENABLE","DISABLE" )
lcValuStat  = IIF((laScrMode[3] .OR. laScrMode[4])  ;
                  .AND. RECCOUNT(lc_TemDet) <> 0 ,"ENABLE","DISABLE")
lcCommStat  = IIF(lcValuStat="ENABLE" .AND. ;
                 (!EMPTY(&lc_TemDet..CDRORCR)),"ENABLE","DISABLE")
lcColPair   = IIF(laScrMode[1] .OR. laScrMode[2] ,SCHEME(1,10),SCHEME(1,2))
lcObjStatus = IIF(laScrMode[2] .AND. laData[2] $ 'EOUZH',"ENABLE","DISABLE")
laCtrStat[7]= lcObjStatus
laCtrStat[8]= lcObjStatus
cbInclEmty  = lnInclEmty
****


*E300683,5 Call *.SPR from screens directory
* DO GlBegBl.SPR 
DO (gcScrDir + gcWinAppl + '\GlBegBl.SPR')
*E300683,5 end   
SELECT GLBATCH
SET FILTER TO

IF glQuitting 
  IF WEXIST("CWRGLBTRDT")
    RELEASE WINDOW CWRGLBTRDT
  ENDIF
  
  *** erase all temparary files 
  IF USED (lc_TemDet)
    USE  IN ALIAS(lc_TemDet)
  ENDIF

  ERASE &gcWorkdir.&lc_TemDet..DBF
  ERASE &gcWorkdir.&lc_TemDet..CDX


  IF USED (lc_MasDet)
    USE IN ALIAS(lc_MasDet)
  ENDIF

  ERASE &gcWorkdir.&lc_MasDet..DBF
  ERASE &gcWorkdir.&lc_MasDet..CDX
ENDIF  
 
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

SELECT GLBATCH

IF laScrMode [2]
  && hold the audit information in lcBStamp from the current record.
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
ENDIF  
=lfCheckRec()  && check the current batch status. 

DO CASE 
  CASE laScrMode [1]           && Select Mode

    *** initialize variables. ***
    lcStatus   = " "
    lcPostPrd  = " "
    lnBalance  = 0.00
    lcBalncID  = " "
    cbHold     = 0

    *** initialize child window variables. ***
    lcTranNo   = " "
    lcTranRef  = " " 
    cbInclEmty = 0
    lcTranDes  = " " 
    lcSourcJor = " " 
    puSourcJor = 0
    lcActType  = " "
    lcTypeID   = " "
    puActtype  = 0
    lcAcctCode = " "
    lnDebit    = 0.00
    lcAccDesc  = " " 
    lnCredit   = 0.00
    lcTrdTexp  = " "

    lncbHold   = cbHold 
    lcSelType  = " " 

    SHOW GET laData[1]  ENABLE
    SHOW GET pbNew      ENABLE
    SHOW GET laData[11] DISABLE
    SHOW GET laData[10] DISABLE
    SHOW GET lnBalance  DISABLE
    SHOW GET lcBalncId  DISABLE
    SHOW GET laData[7]  DISABLE
    SHOW GET cbHold     DISABLE
    SHOW GET laData[8]  DISABLE
    SHOW GET laData[9]  DISABLE
    SHOW GET pbEntries  DISABLE
    SHOW GET pbPrint    DISABLE
    SHOW GET pbPost     DISABLE

    SELECT (lc_MasDet)
    ZAP
    SELECT (lc_TemDet)
    ZAP
    SHOW GET lsBegDet 

    IF WVISIBLE('CWRGLBTRDT')
      =gfChClose('CWRGLBTRDT')
    ENDIF  
    SHOW GET pbDlt,1 &lcPrompt DISABLE
    lcObjStatus = 'DISABLE'
    *B600486,1 Control the status of the delete bar in the record pad.
    laCtrStat[8] = "DISABLE"
    
  CASE laScrMode[2] .OR. laScrMode[3] 
    lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
    cbHold    = IIF(laData[2]='H',1,0)
    lncbHold  = cbHold 
    lnBalance = ABS(laData[11] - laData[10])
    lcBalncID = IIF(laData[11] > laData[10],"Cr",;
                IIF(laData[11] < laData[10],"Dr",""))

    *lnPostPrd  = gfPeriod(laData[5],gcAct_Comp)
    lnPostPrd  = gfPeriod(laData[5],gcPrnt_Cmp)
    lcPostPrd  = RIGHT("0"+ALLTRIM(STR(lnPostPrd)),2) + '-' + laData[4]

    SELECT GLTRNSHD
    lcExactSet = SET("EXACT")
    SET EXACT OFF
    =SEEK (laData[1],'GLTRNSHD')
    lcTranNo   = GLTRNSHD.cTranNo
    lcTranDes  = GLTRNSHD.cTrnDesc
    lcTranRef  = GLTRNSHD.cTrnRefer 

    lnSourcNo  = ASCAN('laSourcJor',GLTRNSHD.cSrcJrnl)
    puSourcJor = lnSourcNo
    IF lnSourcNo > 0
      lcSourcJor = laSourcJor[lnSourcNo]
    ELSE
      lcSourcJor = SPACE(50)
    ENDIF
    SET EXACT &lcExactSet  
      
    lcTypeID   = " "
    lcAcctCode = " " 
    lcAccDesc  = " " 
    lcTrdTexp  = " " 
    lnCredit   = 0.00
    lnDebit    = 0.00
      
    SHOW GET pbEntries  ENABLE

    IF laScrMode [2]                  && View Mode  
      lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
      cbInclEmty = 0
      laAccTypes[1,1] = '----------------- All Types -----------------'
      lcActType =  laAccTypes[1,1]
      puActtype  = 1
 
      SHOW GET laData[1]   DISABLE
      SHOW GET pbNew       ENABLE
      SHOW GET laData[11]  DISABLE
      SHOW GET laData[10]  DISABLE
      SHOW GET lnBalance   DISABLE
      SHOW GET lcBalncId   DISABLE 
      SHOW GET laData[7]   DISABLE
      SHOW GET cbHold      DISABLE
      SHOW GET laData[8]   DISABLE
      SHOW GET laData[9]   DISABLE
      IF laData[2]= "E" 
        SHOW GET pbPrint   DISABLE
      ELSE
        SHOW GET pbPrint   ENABLE
      ENDIF  

      && child window objects.
      SHOW GET lcTranNo   DISABLE
      SHOW GET lcTranRef  DISABLE
      SHOW GET cbInclEmty DISABLE
      SHOW GET lcTranDes  DISABLE
      SHOW GET ibSourJor  DISABLE
      SHOW GET puSourcJor DISABLE    
      SHOW GET ibActType  DISABLE
      SHOW GET puActType  DISABLE
      SHOW GET lcacctcode DISABLE
      SHOW GET lnDebit    DISABLE
      SHOW GET lcAccDesc  DISABLE
      SHOW GET lnCredit   DISABLE
      SHOW GET lcTrdtexp  DISABLE
      SHOW GET pbDTClose  ENABLE
      *** New
      SELECT (lc_TemDet)
      ZAP   && Delete old data ( if any )
      *** Disable error handler until the list is refreshed,then
      *** Enable it again
      lcErrSett      = ON("ERROR")
      ON ERROR lnDum = 1
      SHOW GET lsBegDet 
      ON ERROR &lcErrSett.
      lsBegDet   = 1 
      SELECT GLACCHAR
      lnSelected = 0
      lnTotalRec = RECCOUNT("GLACCHAR")

      SELECT GLTRNSDT.CACCTCODE, GLACCHAR.CACCNLDES, GLACBALS.NACBOPBAL,;
      GLTRNSDT.NAMOUNT AS 'NAMOUNT',GLTRNSDT.CDRORCR AS 'CDRORCR',;
      'S' AS 'CSTATUS',GLTRNSDT.cTrdtexp AS 'cTrdtexp',;
      RECNO() AS 'NRECNO',lfTherm() ;
      FROM GLTRNSDT, GLACCHAR, GLACBALS;
      INTO  DBF &gcWorkDir.&lc_TemDet ;
      WHERE GLACBALS.CACCTCODE +GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID= ;
      GLACCHAR.CACCTCODE+laData[4]+SUBSTR(lcPostPrd,1,2);
      .AND. GLTRNSDT.cBatchNo+GLTRNSDT.cTranNo =  laData[1]+lcTranNo;
      .AND. GLTRNSDT.CACCTCODE = GLACCHAR.CACCTCODE

      IF lnTotalRec <> lnSelected
        FOR lnCounter = lnSelected TO lnTotalRec STEP 10
          =gfThermo(lnTotalRec,lnCounter,'','')
        ENDFOR   
      ENDIF
      =gfThermo(lnTotalRec,lnTotalRec,'','')
 
      SELECT (lc_TemDet)
      INDEX ON  cAcctCode TAG cAccTem
      SET ORDER TO TAG cAccTem
      SHOW GET lsBegDet
      =lfwBegDet()

      IF laData[2] $ 'PA'
        SHOW GET pbPost DISABLE
        SHOW GET pbCopy DISABLE
        SHOW GET pbEdt  DISABLE
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "DISABLE"
        
        SHOW GET pbDlt,1 &lcPrompt DISABLE
        lcObjStatus = 'DISABLE'
        *B600486,1 Control the status of the delete bar in the record pad.
        laCtrStat[8] = "DISABLE"
        
      ELSE
        IF laData[2]= "U"
          SHOW GET pbPost   ENABLE
        ELSE  
          SHOW GET pbPost   DISABLE    
        ENDIF
        SHOW GET pbCopy ENABLE 
        SHOW GET pbEdt  ENABLE 
        *B600486,1 Control the status of the edit bar in the record pad.
        laCtrStat[7] = "ENABLE"
        
        SHOW GET pbDlt,1 &lcPrompt ENABLE
        lcObjStatus = 'ENABLE'
        *B600486,1 Control the status of the delete bar in the record pad.
        laCtrStat[8] = "ENABLE"
      ENDIF

    ELSE                       && Edit Mode  
      cbInclEmty = 1
      laAccTypes = .F.
      laAccTypes[1,1] = 'No Account Type Selected'
      lcActType  =  laAccTypes[1,1]
      puActtype  = 1

      SHOW GET laData[1]  DISABLE
      SHOW GET pbNew      DISABLE
      SHOW GET laData[11] ENABLE
      SHOW GET laData[10] ENABLE
      SHOW GET lnBalance  ENABLE
      SHOW GET lcBalncId  ENABLE
      SHOW GET laData[7]  ENABLE
      IF laData[2] $ 'UH'
        SHOW GET cbHold  ENABLE  
      ELSE  
        SHOW GET cbHold  DISABLE
      ENDIF
      SHOW GET laData[8]  ENABLE     
      SHOW GET laData[9]  ENABLE  
      SHOW GET pbPrint    DISABLE
      SHOW GET pbPost     DISABLE
      
      && child window objects
      SHOW GET lcTranNo   ENABLE
      SHOW GET lcTranRef  ENABLE
      SHOW GET cbInclEmty ENABLE
      SHOW GET lcTranDes  ENABLE
      SHOW GET ibSourJor  ENABLE 
      SHOW GET puSourcJor ENABLE
      SHOW GET ibActType  ENABLE
      SHOW GET puActType  ENABLE
      SHOW GET lcacctcode ENABLE
      SHOW GET lcAccDesc  ENABLE
      SHOW GET pbDTClose  ENABLE

      SELECT * ,'S' AS 'CSTATUS',RECNO() AS 'NRECNO';
      FROM GLTRNSDT;
      INTO  DBF &gcWorkDir.&lc_MasDet ;
      WHERE GLTRNSDT.cBatchNo+GLTRNSDT.cTranNo =  laData[1]+lcTranNo 

      SELECT (lc_MasDet)
      INDEX ON  cAcctCode TAG cAccMas
      SET ORDER TO TAG cAccMas
      lsBegDet   = 1 
      SELECT (lc_TemDet)
      ZAP
      SHOW GET lsBegDet
      =lfwBegDet()
      lcSelType  = "%" 
      SHOW GET pbDlt,1 &lcPrompt DISABLE
      lcObjStatus = 'DISABLE'
      *B600486,1 Control the status of the delete bar in the record pad.
      laCtrStat[8] = "DISABLE"
    ENDIF
 
  CASE laScrMode [4]             && Add Mode
    laData[1]  = ''
    laData[2]  = "E"
    laData[3]  = "B"
    =gfPeriod(GLSETUP.dSetBbDat,gcPrnt_Cmp,@lcYear)
    laData[4]  = lcYear
    laData[5]  = GLSETUP.dSetBbDat
    laData[6]  = GLSETUP.dSetBbDat
    lcStatus   = laTStatus[1]
    laData[11] = 0.00
    laData[10] = 0.00
    *lnPostPrd  = gfPeriod(laData[5],gcAct_Comp)
    lnPostPrd  = gfPeriod(laData[5],gcPrnt_Cmp)
    lcPostPrd  = RIGHT("0"+ALLTRIM(STR(lnPostPrd)),2) + '-' + laData[4]
    lnBalance  = 0
    lcBalncID  = " "
    laData[7]  = 0.00
    cbHold     = 0 
    lncbHold   = cbHold
    laData[8]  = "On " + IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                         LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))
    laData[9]  = SUBSTR("Created by " + LOOKUP(SYUUSER.cUsr_Name,gcUser_Id,SYUUSER.cUser_Id),1,40)
    laData[12] = gcAct_Appl
    laData[13] = gcAct_Comp

    && -----> initialize child window variables
    lcTranNo   = " "
    lcTranRef  = "On " + IIF(SET('CENTURY')='ON',DTOC(gdSysDate),;
                         LEFT(DTOC(gdSysDate),6)+STR(YEAR(gdSysDate),4))
    cbInclEmty = 1
    lcTranDes  = "Beginning balances by " + LOOKUP(SYUUSER.cUsr_Name,gcUser_Id,SYUUSER.cUser_Id)
    lcSourcJor = laSourcJor[ASCAN(laSourcJor,lcSJ_Def),1] 
    puSourcJor =  ASCAN(laSourcJor,lcSJ_Def)
    lcSelType  = " "
    laAccTypes = .F.
    laAccTypes[1,1] = 'No Account Type Selected'
    lcActType  =  laAccTypes[1,1]
    puActtype  = 1
    lcTypeID   = " "
    lcAcctCode = " "
    lnDebit    = 0.00
    lcAccDesc  = " " 
    lnCredit   = 0.00
    lcTrdTexp  = " "

    SELECT (lc_MasDet)
    ZAP
    SELECT (lc_TemDet)
    ZAP
    SHOW GET lsBegDet
    SHOW GET laData[1]  DISABLE
    SHOW GET pbNew      DISABLE
    SHOW GET laData[11] ENABLE  
    SHOW GET laData[10] ENABLE  
    SHOW GET lnBalance  ENABLE  
    SHOW GET lcBalncId  ENABLE  
    SHOW GET laData[7]  ENABLE     
    SHOW GET cbHold     DISABLE
    SHOW GET laData[8]  ENABLE     
    SHOW GET laData[9]  ENABLE  
    SHOW GET pbEntries  ENABLE   
    SHOW GET pbPrint    DISABLE
    SHOW GET pbPost     DISABLE

    && child window objects.
    SHOW GET lcTranNo   DISABLE
    SHOW GET lcTranRef  ENABLE
    SHOW GET cbInclEmty ENABLE
    SHOW GET lcTranDes  ENABLE
    SHOW GET ibSourJor  ENABLE 
    SHOW GET puSourcJor ENABLE 
    SHOW GET ibActType  ENABLE 
    SHOW GET puActtype  ENABLE
    SHOW GET lcacctcode DISABLE
    SHOW GET lnDebit    DISABLE
    SHOW GET lcAccDesc  ENABLE
    SHOW GET lnCredit   DISABLE
    SHOW GET lcTrdtexp  DISABLE
    SHOW GET pbDTClose  ENABLE
    SHOW GET pbDlt,1 &lcPrompt DISABLE
    lcObjStatus = 'DISABLE'
    *B600486,1 Control the status of the delete bar in the record pad.
    laCtrStat[8] = "DISABLE"
ENDCASE

SHOW GET ibBgDumi1   ENABLE
SHOW GET ibEndDumi1  ENABLE
SHOW GET ibBgDumi2   ENABLE
SHOW GET ibEndDumi2  ENABLE
SHOW GET ibFrstObj   ENABLE
SHOW GET ibLastObj   ENABLE
SHOW GET pbDtClose   ENABLE
lnInclEmty = cbInclEmty
lcColPair= IIF(laScrMode[1] .OR. laScrMode[2] ,SCHEME(1,10),SCHEME(1,2))
SELECT GLBATCH


*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()= 13)
  IF llBrowse
    =gfSeekRec()
  ELSE
    IF LEFT(laData[1],1) <> '?'
      laData[1] = RIGHT("000000"+ALLTRIM(laData[1]),6)
      SHOW GET laData[1]
    ENDIF  
  
    IF RECNO() <= RECCOUNT() .AND. ! EOF()
      GOTO RECNO()  
    ENDIF  
    =gfSeekRec()

    IF laScrMode[4]    && add mode
     laData[1] = "      "
     laData[2] = 'E'
     SHOW GET laData[1]
    ENDIF
  ENDIF
ENDIF
llBrowse = .F.
 
*!**************************************************************************
*!
*!      Function: lfvData_7
*!
*!**************************************************************************
*
FUNCTION lfvData_7
 
*** check the value of audit total
IF SIGN(laData[7]) = -1
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  laData[7] = 0.00
  _CUROBJ = OBJNUM(laData[7])
ENDIF 
   
*!**************************************************************************
*!
*!      Function: lfvPrint
*!
*!**************************************************************************
*
FUNCTION lfvPrint

IF ! lfCheckRec()  && check the current batch status.
  RETURN
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvPost
*!
*!**************************************************************************
*
FUNCTION lfvPost

IF ! lfCheckRec()   && check the current batch status.
  RETURN
ENDIF  

*** check if the batch year falls in posting window.
IF ASCAN(laFisYear,glBatch.cbatpyr) <> 0

  *** check if the user print the batch after last updating. 
  IF !EMPTY(glBatch.cBatElUsr) 
    llEditList= IIF ( glBatch.dbaTelDat > glBatch.daDd_Date,.T.,;
                IIF ( glBatch.dbaTelDat = glBatch.daDd_Date .AND.;
                      glBatch.cbAtelTim > glBatch.caDd_Time,.T.,.F.))
  ELSE 
    llEditList = .F. 
  ENDIF    

  *** check the force printing flag in glsetup
  llPrint = IIF (glSetup.lSetForel,;
            IIF (llEditList,.T.,.F.),.T.)
            
  IF llPrint 
    *** Are you sure you want to post this ð ?
    *** <  Post  > - <  No  > ***
    IF gfModalGen("TRM02149B02009","DIALOG",'batch') = 1 
      IF ! lfCheckRec()   && check the current batch status.
        RETURN
      ENDIF  
      
      SELECT GLBATCH 
      IF gfObj_Lock(.T.)
        IF ! lfCheckRec()   && check the current batch status.
          RETURN
        ENDIF  
        *** calling posting program.
        IF lfTBPost('BATCH','','Beginning') <> 0 
          SCATTER FIELDS &lcScFields MEMO TO laData
          
          *** update the audit information in the variable lcBStamp. ***
          lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
          laScrMode    = .F.
          laScrMode[2] = .T.   
          SHOW GETS            
        ENDIF
        =gfObj_Lock(.F.)  
      ENDIF

    ENDIF
  ELSE
    *** Printing the edit list is required to post this batch. ***
    *** <  Ok  > ***
    =gfModalGen("TRM02148B00000","Dialog")
  ENDIF  
ELSE   
  *** The ð posting year is out of the posting window. ***
  *** You cannot post this ð. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02147B00000","Dialog",'batch|batch')
ENDIF

*!**************************************************************************
*!
*!      Function: lfvHold
*!
*!**************************************************************************
*
FUNCTION lfvHold

lncbHold = cbHold 

IF cbHold = 1
  laData[2]  = 'H'
  lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
 ELSE
  laData[2]  = 'U'
  lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
ENDIF

=lfRefresh()
  
*!**************************************************************************
*!
*!      Function: lfvComment
*!
*!**************************************************************************
*
FUNCTION lfvComment

SELECT (lc_TemDet)
REPLACE &lc_TemDet..cTrdtexp WITH lcTrdtexp ;
        &lc_TemDet..CSTATUS  WITH 'M'
SELECT GLBATCH

*!**************************************************************************
*!
*!      Function: lfwDebit
*!
*!**************************************************************************
*
FUNCTION lfwDebit

lnOldDebi  = lnDebit   

*!**************************************************************************
*!
*!      Function: lfvDebit
*!
*!**************************************************************************
*
FUNCTION lfvDebit

*IF LASTKEY() <> 13
*  lnDebit = lnOldDebi
*  SHOW GET lnDebit
*  RETURN
*ENDIF

*** check if the current account (if it's allowed for posting 
*** and an active account)
IF lnDebit <> 0 .AND. ! lfValidAcct()
  RETURN
ENDIF

SELECT (lc_TemDet)

IF lnDebit < 0 
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  lnDebit = lnOldDebi
  _CUROBJ = OBJNUM(lnDebit)    
  SHOW GET lcTrdtexp ENABLE
ELSE  
  REPLACE &lc_TemDet..CSTATUS WITH 'M'
  IF lnDebit > 0 
    laData[10] = laData[10] - lnCredit
    laData[11] = laData[11] - lnOldDebi
    laData[11] = laData[11] + lnDebit
    lnCredit  = 0 
    REPLACE &lc_TemDet..NAMOUNT WITH lnDebit ;
            &lc_TemDet..CDRORCR WITH 'D'
    SHOW GET lcTrdtexp ENABLE
  ELSE  
    IF &lc_TemDet..CDRORCR = 'D'
      laData[11] = laData[11] - lnOldDebi 
      lnDebit   = 0     
      lnCredit  = 0 
      REPLACE &lc_TemDet..NAMOUNT WITH lnDebit ;
              &lc_TemDet..CDRORCR WITH ' '
      SHOW GET lcTrdtexp DISABLE
    ENDIF  
  ENDIF  
ENDIF

lnBalance = ABS(laData[11] - laData[10])
lcBalncID = IIF(laData[11] > laData[10],"Cr",;
            IIF(laData[11] < laData[10],"Dr",""))

IF laData[11] <> laData[10]
  laData[2] = 'O'
  cbHold      = 0 
  lncbHold    = 0 
  SHOW GET cbHold DISABLE
ELSE
  IF laData[11] = 0 
    laData[2]   = 'E'
    cbHold      = 0 
    lncbHold    = 0 
    SHOW GET cbHold DISABLE
  ELSE
    IF lncbHold = 1  
      laData[2] = 'H'  
      SHOW GET cbHold DISABLE
    ELSE  
      laData[2] = 'U'  
      SHOW GET cbHold ENABLE 
    ENDIF  
  ENDIF  
ENDIF 

lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
=lfRefresh()

SHOW GET lnDebit
SHOW GET lnCredit
SHOW GET lsBegDet
SHOW GET laData[10]
SHOW GET laData[11]
SHOW GET lnBalance
SHOW GET lcBalncID

=lfwBegDet()

*!**************************************************************************
*!
*!      Function: lfwCredit
*!
*!**************************************************************************
*
FUNCTION lfwCredit

lnOldCred  = lnCredit

*!**************************************************************************
*!
*!      Function: lfvCredit
*!
*!**************************************************************************
*
FUNCTION lfvCredit

*IF LASTKEY() <> 13
*  lnCredit = lnOldCred
*  SHOW GET lnCredit
*  RETURN
*ENDIF

*** check if the current account (if it's allowed for posting 
*** and an active account)
IF lnCredit <> 0 .AND. ! lfValidAcct()
  RETURN
ENDIF

SELECT (lc_TemDet)
IF lnCredit < 0 
  *** Negative values are not allowed. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02036B00000","DIALOG")
  lnCredit = lnOldCred
  _CUROBJ  = OBJNUM(lnCredit)    
  SHOW GET lcTrdtexp ENABLE
ELSE  
  REPLACE &lc_TemDet..CSTATUS WITH 'M'
  IF lnCredit > 0 
    laData[11] = laData[11] - lnDebit
    laData[10] = laData[10] - lnOldCred
    laData[10] = laData[10] + lnCredit
    lnDebit   = 0 
    REPLACE &lc_TemDet..NAMOUNT WITH lnCredit ;
            &lc_TemDet..CDRORCR WITH 'C'
    SHOW GET lcTrdtexp ENABLE
  ELSE  
    IF &lc_TemDet..CDRORCR = 'C'
      laData[10] = laData[10] - lnOldCred      
      lnDebit   = 0     
      lnCredit  = 0 
      REPLACE &lc_TemDet..NAMOUNT WITH lnDebit ;
              &lc_TemDet..CDRORCR WITH ' '
      SHOW GET lcTrdtexp DISABLE
    ENDIF  
  ENDIF  
ENDIF

lnBalance = ABS(laData[11] - laData[10])
lcBalncID = IIF(laData[11] > laData[10],"Cr",;
            IIF(laData[11] < laData[10],"Dr",""))

IF laData[11] <> laData[10]
  laData[2] = 'O'
ELSE
  IF laData[11] = 0 
    laData[2]   = 'E'
    cbHold      = 0 
    lncbHold    = 0 
    SHOW GET cbHold 
  ELSE
    IF lncbHold = 1  
      laData[2] = 'H'  
    ELSE  
        laData[2] = 'U'  
    ENDIF  
  ENDIF  
ENDIF 

lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
=lfRefresh()

SHOW GET lnDebit
SHOW GET lnCredit
SHOW GET lsBegDet
SHOW GET laData[10]
SHOW GET laData[11]
SHOW GET lnBalance
SHOW GET lcBalncID

=lfwBegDet()

*!**************************************************************************
*!
*!      Function: lfwBegDet 
*!
*!**************************************************************************
*
FUNCTION lfwBegDet

lcAcctCode = &lc_TemDet..cAcctCode
lcAccDesc  = &lc_TemDet..cAccNlDes
lcTrdtexp  = &lc_TemDet..cTrdtexp 

IF &lc_TemDet..CDRORCR = 'C'
  lnCredit  = &lc_TemDet..nAmount  
  lnDebit   = 0 
ELSE 
  lnDebit   = &lc_TemDet..nAmount  
  lnCredit  = 0 
ENDIF

IF RECCOUNT(lc_TemDet) = 0 .OR. laScrMode[2]
  SHOW GET lcTrdtexp DISABLE
  SHOW GET lnDebit   DISABLE
  SHOW GET lnCredit  DISABLE
ELSE
  SHOW GET lnDebit   ENABLE
  SHOW GET lnCredit  ENABLE 
  IF &lc_TemDet..CDRORCR $ 'CD'
    SHOW GET lcTrdtexp ENABLE
  ELSE
    SHOW GET lcTrdtexp DISABLE
  ENDIF
ENDIF

IF  EOF(lc_TemDet)
  SHOW GET lnDebit    DISABLE
  SHOW GET lnCredit   DISABLE
ENDIF

SHOW GET lcAcctCode DISABLE
SHOW GET lcAccDesc  DISABLE
SELECT GLBATCH

*!**************************************************************************
*!
*!      Function: lfvDeClose
*!
*!**************************************************************************
*
FUNCTION lfvDeClose

=lfSwapT2M()
=gfChClose()

*!**************************************************************************
*!
*!      Function: lfSwapM2T
*!
*!**************************************************************************
*
FUNCTION lfSwapM2T

SELECT (lc_TemDet)
SET FILTER TO 

SELECT (lc_MasDet)

SCAN ALL 
  IF SEEK (&lc_MasDet..cAcctCode,lc_TemDet)
    SELECT (lc_TemDet)
    REPLACE ;
      &lc_TemDet..nAmount   WITH &lc_MasDet..nAmount   ;
      &lc_TemDet..cDrOrCr   WITH &lc_MasDet..cDrOrCr   ;
      &lc_TemDet..cTrdtexp  WITH &lc_MasDet..cTrdtexp  
  ENDIF
  SELECT (lc_MasDet)
ENDSCAN

SELECT (lc_TemDet)
IF cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ENDIF  

SELECT GlBATCH

*!**************************************************************************
*!
*!      Function: lfSwapT2M
*!
*!**************************************************************************
*
FUNCTION lfSwapT2M

SELECT (lc_TemDet)
SET FILTER TO 

SCAN ALL FOR &lc_TemDet..CSTATUS = 'M' 
  SELECT (lc_MasDet)
  IF SEEK (&lc_TemDet..cAcctCode,lc_MasDet)
    REPLACE ;
      &lc_MasDet..cAcctCode WITH &lc_TemDet..cAcctCode ;
      &lc_MasDet..nAmount   WITH &lc_TemDet..nAmount ;
      &lc_MasDet..cDrOrCr   WITH &lc_TemDet..cDrOrCr ;
      &lc_MasDet..cTrdtexp  WITH &lc_TemDet..cTrdtexp ;
      &lc_MasDet..cTrnpYr   WITH laData[4] ;
      &lc_MasDet..DtrnpDate WITH laData[5] ;
      &lc_MasDet..cTrnpPrd  WITH lcPostPrd ;           
      &lc_MasDet..cStatus   WITH 'M'
  ELSE 
    INSERT INTO &gcWorkDir.&lc_MasDet ;
           (cAcctCode,nAmount,cTrdtexp,cDrOrCr,;
           cTrnpYr,DtrnpDate,cTrnpPrd,cStatus);
    VALUES (&lc_TemDet..cAcctCode, ;
           &lc_TemDet..nAmount, ;
           &lc_TemDet..cTrdtexp, ;              
           &lc_TemDet..cDrOrCr, ;
           laData[4],laData[5], ;
           lcPostPrd,'A')
  ENDIF
  SELECT (lc_TemDet)
ENDSCAN

SELECT (lc_TemDet)
IF cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ENDIF  

SELECT GlBATCH

*!**************************************************************************
*!
*!      Function: lfwSelType
*!
*!**************************************************************************
*
FUNCTION lfwSelType

SELECT  cTypeCode+" "+cTypeDesc;
  FROM GLTYPES;
  INTO ARRAY laAccTypes

DIMENSION laAccTypes[ALEN(laAccTypes,1)+1,1]
=AINS(laAccTypes,1)
laAccTypes[1,1] = '----------------- All Types -----------------'

DO CASE
  CASE _DOS
    =lfRefresh()
  CASE _WINDOWS
    SHOW GET puActtype 
ENDCASE

*!**************************************************************************
*!
*!      Function: lfvSelType
*!
*!**************************************************************************
*
FUNCTION lfvSelType

DO CASE
  CASE _DOS
    = gfActPop(3,18,IIF(ALEN(laAccTypes) > 8,11;
      ,ALEN(laAccTypes)+5),66,'laAccTypes',1,1,@lcActType)
  CASE _WINDOWS
    lcActType = laAccTypes[puActtype,1]
    SHOW GET puActtype 
ENDCASE

IF SUBSTR(lcActType,1,3) = lcSelType
  RETURN
ELSE
  =lfSwapT2M()  
ENDIF 

IF SUBSTR(lcActType,1,3) = "---" 
  lcSelType =  '%'
ELSE
  IF SUBSTR(lcActType,2,3) = "00" 
    lcSelType = SUBSTR(lcActType,1,1) + '%'
  ELSE
    lcSelType = SUBSTR(lcActType,1,3)
  ENDIF
ENDIF

IF SUBSTR(lcActType,1,1) $ 'AECT'
  lcTypeId = 'Dr'
ELSE
  IF SUBSTR(lcActType,1,1) $ 'LQSI'   
    lcTypeId = 'Cr'
  ELSE  
    lcTypeId = '  '
  ENDIF
ENDIF    

=lfRefresh()
 
****  New 
SELECT (lc_TemDet)
ZAP   && Delete old data ( if any )
*** Disable error handler until the list is refreshed,then
*** Enable it again
lcErrSett      = ON("ERROR")
ON ERROR lnDum = 1
SHOW GET lsBegDet 
ON ERROR &lcErrSett.
****
 
SELECT GLACCHAR
lnSelected = 0
lnTotalRec = RECCOUNT("GLACCHAR")

SELECT GLACCHAR.CACCTCODE, GLACCHAR.CACCNLDES, GLACBALS.NACBOPBAL,;
    000000000000.00 AS 'NAMOUNT',' ' AS 'CDRORCR','S' AS 'CSTATUS',;
    SPACE(40) AS 'cTrdtexp',RECNO() AS 'NRECNO',lfTherm() ;
    FROM GLACCHAR, GLACBALS;
    INTO  DBF &gcWorkDir.&lc_TemDet ;
    WHERE GLACBALS.CACCTCODE +GLACBALS.CFISFYEAR+GLACBALS.CFSPPRDID= ;
    GLACCHAR.CACCTCODE+laData[4]+SUBSTR(lcPostPrd,1,2);
    .AND. GLACCHAR.CTYPECODE LIKE lcSelType 

IF lnTotalRec <> lnSelected
  FOR lnCounter = lnSelected TO lnTotalRec STEP 10 
    =gfThermo(lnTotalRec,lnCounter,'','')
  ENDFOR   
ENDIF

=gfThermo(lnTotalRec,lnTotalRec,'','')

IF SUBSTR(lcSelType,2,1) = '%'
  lcSelType = SUBSTR(lcActType,1,1) + '00'
ELSE
  IF SUBSTR(lcSelType,1,1) = '%'
    lcSelType =  '---'
  ENDIF
ENDIF

lnDebit   = 0 
lnCredit  = 0 
lcTrdtexp = " " 

SELECT (lc_TemDet)
INDEX ON  cAcctCode TAG cAccTem
SET ORDER TO TAG cAccTem
=lfSwapM2T()  
=lfvInclEmp()

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

IF laScrMode [2]
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF  
laData[1] = "" 
laScrMode    = .F.
laScrMode[4] = .T.
SHOW GETS

*!**************************************************************************
*!
*!      Function: lfvEntries
*!
*!**************************************************************************
*
FUNCTION lfvEntries

IF ! lfCheckRec()   && check the current batch status.
  RETURN
ENDIF  

=gfActWind('CWRGLBTRDT',lc_Title)

*!**************************************************************************
*!
*!      Function: lfwSourJor
*!
*!**************************************************************************
*
FUNCTION lfwSourJor

SELECT  cSrcJrnl+" "+cJorlnDes;
        FROM &gcDataDir.GLSUBJOR;
        INTO ARRAY laSourcJor

IF ALEN(laSourcJor,1) = 1 .AND. TYPE('laSourcJor[1]') = 'L'
  laSourcJor = " "
  laSourcJor[1,1] = lcSJ_Def
ELSE
  IF ASCAN(laSourcJor,lcSJ_Def) = 0
    DIMENSION laSourcJor[ALEN(laSourcJor,1)+1,1]
    =AINS(laSourcJor,1)
    laSourcJor[1,1] = lcSJ_Def
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSourJor
*!
*!**************************************************************************
*
FUNCTION lfvSourJor

DO CASE
  CASE _DOS
    =ALLTRIM(gfActPop(2,18,IIF(ALEN(laSourcJor) > 8,10;
    ,ALEN(laSourcJor)+4),66,'laSourcJor',1,1,@lcSourcJor))
    =lfRefresh()
  CASE _WINDOWS
    lcSourcJor  = laSourcJor[puSourcJor,1]
    SHOW GET puSourcJor 
ENDCASE    

*!**************************************************************************
*!
*!      FUNCTION: lfvInclEmp
*!
*!**************************************************************************
*
FUNCTION lfvInclEmp

SELECT (lc_TemDet)

IF cbInclEmty = 0 
  SET FILTER TO ! EMPTY(&lc_TemDet..nAmount)
ELSE 
  SET FILTER TO 
ENDIF  

SHOW GET lsBegDet
lnInclEmty = cbInclEmty
=lfwBegDet()

*!**************************************************************************
*!
*!      Procedure: lpDelScr
*!
*!**************************************************************************
*
PROCEDURE lpDelScr

IF lfCheckRec()   && check the current batch status.
  laScrMode = .F.
  laScrMode [1] = .T.  
  REPLACE GLBATCH.cBatStat WITH 'V'
  =gfAdd_Info('GLBATCH')
  =gfObj_Lock(.F.) 
ENDIF  

*!**************************************************************************
*!
*!      FUNCTION: lfValidAcct
*!
*!**************************************************************************
*
*
FUNCTION lfValidAcct

llValid = .T.

SELECT GLACCHAR
SET ORDER TO ACCTCODE
=SEEK (&lc_TemDet..cAcctCode)

IF GLACCHAR.cSegActiv = 'I'
  *** Account ð is Inactive. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02052B00000","DIALOG",ALLTRIM(&lc_TemDet..cAcctCode))
  llValid = .F.
ENDIF

IF GLACCHAR.cSegAlPos = 'N'
  *** Account ð is not allowed for posting from G.L. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02054B00000","DIALOG",ALLTRIM(&lc_TemDet..cAcctCode))
  llValid = .F.
ENDIF

IF llValid 
  RETURN llValid
ELSE
  SELECT (lc_TemDet)
  lnDebit   = 0     
  lnCredit  = 0 

  IF &lc_TemDet..CDRORCR = 'D'
    laData[11] = laData[11] - &lc_TemDet..NAMOUNT 
  ELSE
    laData[10] = laData[10] - &lc_TemDet..NAMOUNT
  ENDIF  

  REPLACE &lc_TemDet..NAMOUNT WITH lnDebit ;
          &lc_TemDet..CDRORCR WITH ' '     ;
          &lc_TemDet..CSTATUS WITH 'M'

  lnBalance = ABS(laData[11] - laData[10])
  lcBalncID = IIF(laData[11] > laData[10],"Cr",;
              IIF(laData[11] < laData[10],"Dr",""))

  IF laData[11] <> laData[10]
    laData[2] = 'O'
    cbHold      = 0 
    lncbHold    = 0 
    SHOW GET cbHold DISABLE
  ELSE
    IF laData[11] = 0 
      laData[2]   = 'E'
      cbHold      = 0 
      lncbHold    = 0 
      SHOW GET cbHold DISABLE
    ELSE
      IF lncbHold = 1  
        laData[2] = 'H'  
        SHOW GET cbHold DISABLE
      ELSE  
        laData[2] = 'U'  
        SHOW GET cbHold ENABLE 
      ENDIF  
    ENDIF  
  ENDIF 

  lcStatus  = laTStatus [AT(laData[2],'EOUPZHA')]
  =lfRefresh()

  SHOW GET lnDebit
  SHOW GET lnCredit
  SHOW GET lsBegDet
  SHOW GET lcTrdtexp     DISABLE
  SHOW GET laData[10]
  SHOW GET laData[11]
  SHOW GET lnBalance
  SHOW GET lcBalncID
  =lfwBegDet()
  RETURN llValid
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfTherm
*!
*!**************************************************************************
*
FUNCTION lfTherm

lnSelected = lnSelected + 1 

IF lnSelected <> 1    && First Time
  =gfThermo(lnTotalRec,lnSelected,'Collecting batch lines...','')
ENDIF  
	
*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*** local procedure to save the current batch
*
PROCEDURE lpSavScr

*** check if the user is forced to enter the audit total filed 
*** in case of ladata[7] is empty.

IF glSetUp.lSetCnTot .AND. EMPTY(laData[7])
  *** You have to enter the audit total. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02146B00000","DIALOG")
   llcSave = .F.
  _CUROBJ = OBJNUM(laData[7])
  RETURN 
ENDIF

=lfSwapT2M()

SELECT (lc_MasDet)
DELETE ALL FOR EMPTY(&lc_MasDet..nAmount)  .AND.  ;
               EMPTY(&lc_MasDet..nRecNo )
           
IF laData[2] <> 'H'   && HOLD
  IF laData[11] = laData[10]
    IF laData[10] = 0 
      laData[2] = 'E'
    ELSE   
      laData[2] = 'U'
    ENDIF
  ELSE  
    laData[2] = 'O'
  ENDIF  
ENDIF  

lcTime = gfGetTime() 
ldDate = DATE()
SELECT (lc_MasDet)

REPLACE ALL &lc_MasDet..cAdd_User WITH gcUser_Id  ;
            &lc_MasDet..dAdd_Date WITH ldDate     ;
            &lc_MasDet..cAdd_Time WITH lcTime     ;
            &lc_MasDet..cStatus  WITH    ;
               IIF(EMPTY(&lc_MasDet..nRecNo ),'A',;
               IIF(EMPTY(&lc_MasDet..nAmount),'D',&lc_MasDet..cStatus))    

IF laScrMode[4]
  SELECT GLBATCH
  APPEND BLANK

  *E300663,1 Change this line for the changes we have 
  *          made to (gfSequence) [Begin]
  *laData[1]  = RIGHT("000000"+ALLTRIM(STR(gfSequence("BATCH",1))),6)
  laData[1]  = gfSequence('CBATCHNO')
  *E300663,1 Change this line [End]
  
  SHOW GET laData[1] 

  *E300663,1 Change this line for the changes we have 
  *          made to (gfSequence) [Begin]
  *lcTranNo   = RIGHT("00000000"+ALLTRIM(STR(gfSequence("TRANSACT",1))),8)
  lcTranNo   = gfSequence('CTRANNO')
  *E300663,1 Change this line [End]
  
  SHOW GET lcTranNo

  SELECT (lc_MasDet)
  REPLACE ALL &lc_MasDet..cBatchNo  WITH laData[1] ;
              &lc_MasDet..cTranNo   WITH lcTranNo 

  SELECT GLBATCH  
  GATHER FROM laData FIELDS &lcScFields 
  
  INSERT INTO &gcDataDir.GLTRNSHD   ;
             (cBatchNo,cTranNo,cTrnDesc,cTrnRefer, ;
              dTrnpDate,cTrnpYr,cTrnpPrd,cTrnStat, ;
              cTrnType,nTrnTotDr,nTrnTotCr,cSrcJrnl, ;
              cSrcModul,cComp_ID,cTrnRever,cStandard, ;              
              cAdd_User,dAdd_Date,cAdd_Time) ;
      VALUES (laData[1],lcTranNo,lcTranDes,lcTranRef, ;
              laData[5],laData[4],lcPostPrd,laData[2], ;
              laData[3],laData[11],laData[10],SUBSTR(lcSourcJor,1,2), ;
              gcAct_Appl,gcAct_Comp,'N','Y',gcUser_Id,ldDate,lcTime)
  
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  SELECT GLTRNSDT
  APPEND FROM &gcWorkDir.&lc_MasDet

  *** Beginning balance batch saved with number ð. ***
  *** <  Ok  > 
  =gfModalGen("TRM02161B00000","DIALOG",laData[1])
ELSE
  SELECT (lc_MasDet)
  REPLACE ALL &lc_MasDet..cBatchNo  WITH laData[1] ;
              &lc_MasDet..cTranNo   WITH lcTranNo 
  DELETE FOR  &lc_MasDet..cStatus = "D" 

  SELECT GLBATCH  
  GATHER FROM laData FIELDS &lcScFields 
  
  SELECT  GLTRNSHD
 
  REPLACE GLTRNSHD.cTrnDesc   WITH lcTranDes ;
          GLTRNSHD.cTrnRefer  WITH lcTranRef ;
          GLTRNSHD.cTrnStat   WITH laData[2] ;
          GLTRNSHD.nTrnTotDr  WITH laData[11];
          GLTRNSHD.nTrnTotCr  WITH laData[10];
          GLTRNSHD.cSrcJrnl   WITH SUBSTR(lcSourcJor,1,2) ;
          GLTRNSHD.cTrnRever  WITH 'N'       ;
          GLTRNSHD.cStandard  WITH 'Y'       ; 
          GLTRNSHD.cAdd_User  WITH gcUser_Id ;
          GLTRNSHD.dAdd_Date  WITH ldDate    ;
          GLTRNSHD.cAdd_Time  WITH lcTime    ; 

  SELECT GLBATCH
  =gfTmp2Mast('GLTRNSDT',lc_MasDet)
ENDIF

SELECT GLBATCH

*!**************************************************************************
*!
*!      Procedure: lfCheckRec
*!
*!**************************************************************************
*
PROCEDURE lfCheckRec

lcLoclShow = "lpShow"
SHOW GET pbDlt,1 &lcPrompt &lcObjStatus
*B600486,1 Control the status of the delete bar in the record pad.
laCtrStat[8] = lcObjStatus

*** function to check the batch status.

IF ! laScrMode[2] 
  RETURN
ENDIF  

IF EMPTY(laData[1])
  RETURN
ENDIF

IF RECNO() <= RECCOUNT()
  GOTO RECNO()  
ENDIF  

*** check if the displayed batch is removed (voided) by another workstation.
IF laData[1] <> glbatch.cBatchNo
  *** This ð was ð by another user . ***
  *** <  Ok  > ***
  =gfModalGen("TRM02187B00000","DIALOG","batch"+'|'+"voided")
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  SHOW GETS   
  RETURN .F.
ENDIF

IF ( lcBStamp = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time ) ;
   .AND. ( laData[2] = glbatch.cbatstat )
  RETURN .T.
ENDIF

lcAdd_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cAdd_User,SYUUSER.cUser_Id))
lcAdd_Time = glBatch.cAdd_Time
lcAdd_Date = dtoc(glBatch.dAdd_Date)
lcPos_User = ALLTRIM(LOOKUP(SYUUSER.cUsr_Name,glBatch.cPostUser,SYUUSER.cUser_Id))
lcPos_Time = glBatch.cPostTime
lcPos_Date = dtoc(glBatch.dPostDate)

=gfObj_Lock(.F.) 

IF glbatch.cbatstat = 'V' 
  *** This ð was ð by user ð at ð. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"voided"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  laScrMode = .F.
  laScrMode [1] = .T.   
  SHOW GETS   
  RETURN .F.
ENDIF

*** check if the displayed batch was approved by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'A'
  *** This ð was ð by user ð at ð. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"approved"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  laScrMode    = .F.
  laScrMode[3] = .T.
  SHOW GETS 
  RETURN .F.
ENDIF

*** check if the displayed batch was posted by another user 
*** from another or same workstaion.
IF glbatch.cbatstat = 'P'
  *** This ð was ð by user ð at ð. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02167B00000","DIALOG",;
     "batch"+'|'+"posted"+'|'+lcPos_User+'|'+lcPos_Date+' '+lcPos_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp     = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
  laScrMode    = .F.
  laScrMode[3] = .T.   
  SHOW GETS 
  RETURN .F.
ENDIF

IF glbatch.cbatstat $ 'HOEU'
  *** ð status has been changed by user ð at ð. ***
  *** <  Ok  > ***
  =gfModalGen("TRM02181B00000","DIALOG",;
  "batch"+'|'+lcAdd_User+'|'+lcAdd_Date+' '+lcAdd_Time)
  SCATTER FIELDS &lcScFields MEMO TO laData 
  lcBStamp  = DTOC(glbatch.dAdd_Date) + glbatch.cAdd_Time 
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

IF glbatch.cbatstat $ 'APV'
  SHOW GETS
ELSE
  *B607000,1 WAB  (Start)
  *DO gfCpEdit IN ARIA27
  DO gfCpEdit IN AAS_WIN.EXE
  *B607000,1 WAB (END) 
ENDIF
