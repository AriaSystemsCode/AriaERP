*:----------------------------------------------------------------
*: Program file        : ARDCAJSV
*: Program description : Saving the Debit/Credit Adj. Transcation
*: For screen          : 
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Ahmed Amer
*: Tracking Job Number : E301521,1
*:----------------------------------------------------------------
*: Calls               : 
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO lfSavDCAdj IN ARDCAJSV.PRG
*:----------------------------------------------------------------

*:----------------------------------------------------------------
*: Function            : lfSavDCAdj
*: Program description : Saving the Debit/Credit Adj. Transcation
*: For screen          : 
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Account Receivable - (AR)
*: Developer Name      : Ahmed Amer
*: Tracking Job Number : E301521,1
*:----------------------------------------------------------------
*: Calls               : 
*:----------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------
*: Example             : DO lfSavDCAdj IN ARDCAJSV.PRG
*:----------------------------------------------------------------

FUNCTION lfSavDCAdj
PARAMETERS llFromEdi

*E301521,1 (Start) Open the needed tables for NC module use
llEdiAccount = .F.
lcSysType = gfGetMemVar('M_SYSTYPE',gcAct_Comp)
IF !llFromEdi AND 'NC' $ gcCmpModules AND lcSysType = 'P'
  =gfOpenFile(gcDataDir+'EDIACPRT',gcDataDir+'ACCFACT','SH')
  =gfOpenFile(gcDataDir+'EDIPD',gcDataDir+'PARTTRANS','SH')
  =gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')

  SELECT CODES
  SET ORDER TO TAG Idrltfname
  =SEEK('NYCSITEID')
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  IF FOUND()
    lcSiteId = Codes.cCode_No
    SELECT EDiAcPrt
    LOCATE FOR cSiteId = lcSiteId
    IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'812','EDIPD')
      llEdiAccount = .T.
    ENDIF
  ENDIF
ENDIF    
*E301521,1 (End)

SELECT (lcAdjTemp)
LOCATE FOR EMPTY(Account) .OR. IIF(MCHOICE = "D" , EMPTY(TranCode) , EMPTY(cCreditCod))
IF FOUND()
  GO TOP
  *** There is one or more code(s) is missing.  Cannot update! ***
  *** < Ok > ***
  =gfModalGen("INM40078B00000" , "DIALOG")
  _CUROBJ = OBJNUM(lcAccount)
  RETURN
ENDIF

STORE 0 TO lnCount , lnZero

*-- Get the transaction no.
COUNT FOR !DELETED() .AND. &lcAdjTemp..AMOUNT <> 0 TO lnCount

*-- Get the transaction amount.
COUNT FOR &lcAdjTemp..AMOUNT = 0 TO lnZero

IF lnZero >= 1
  *** One or more transaction(s) has zero amount. ***
  *** Transaction(s) with zero amount will be ignored. ***
  *** < OK > ***
  =gfModalGen("INM40071B00000" , "DIALOG")
ENDIF

IF lnCount = 0
  *** There is no transactions entered. ***
  *** < OK > ***
  =gfModalGen("INM40083B00000" , "DIALOG")
  _CUROBJ = OBJNUM(PBNEW)
  RETURN
ENDIF

*-- If the budget amount not equal the actual amount.
IF ((lnActual <> lnAmount) .OR. (lnTrnCnt <> lnCount) .OR. lnTrnCnt=0 .OR. lnAmount=0)
  DO CASE
  CASE lnActual <> lnAmount
    *** Actual amount not equal Tape amount. ***
    *** < Modify> - < Continue > ***
    lnOption = gfModalGen("QRM40079B40009" , "DIALOG")
  CASE lnTrnCnt <> lnCount
    lcCount = "(" + ALLTRIM(STR(lnCount)) + ")"
    *** Actual count "+ lcCount +" not equal Tape count. ***
    *** < Modify> - < Continue > ***
    lnOption = gfModalGen("QRM40080B40009" , "DIALOG" , lcCount)
  ENDCASE
  IF lnOption = 1
    _CUROBJ = OBJNUM(PBNEW)
    RETURN
  ENDIF
ENDIF

IF EMPTY(lcBatchNo)
  *-- Get the batch no.
  lcBatchNo = gfsequence('BATCH')
ENDIF

*-- Save the save push button in the session processes.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE cCurrObj WITH 'pbSav'
  UNLOCK
  =RLOCK()
ENDIF

*-- Update the needed variabnles.
llNoThing = lfUpdVars()

*** Batch No.  : "+lcBatchNo + CHR(13) + "Batch Date : "+DTOC(ldBatchDt) ***
*** < Ok > ***
lcTmpStr = lcBatchNo + "|" + DTOC(ldBatchDt)
=gfModalGen("INM40081B00000" , "DIALOG" , lcTmpStr)

*-- Flag to know if enter the add mode before or not.
llAddMode  = .F.

*-- Check if there is salesreps' charge backs or not.
llUpdRep = .F.
IF llRepCB
  SELECT (lcRepCBTmp)
  DELETE ALL FOR AMOUNT = 0
  GOTO TOP
  llUpdRep = IIF(EOF() , .F. , .T.)
ENDIF

*-- update the master credit or debit file with the entered transaction.
lnCurRec  = 1
lnTotRec  = lnCount
SELECT (lcAdjTemp)
SET ORDER TO
GO TOP
SCAN FOR !DELETED() .AND. AMOUNT <> 0
  SELECT (lcAdjTemp)
  *-- Call the global function that execute the thermometer.
  =gfTherm (lnTotRec,lnCurRec,"Updating the " + lcMode + "File.")
  lnCurRec = lnCurRec + 1
  IF &lcAdjTemp..nSteps < 1
    *-- Call the global sequence function to get transaction no.
    *lcTranNo = IIF(MCHOICE = "D" , gfsequence('DEBIT') , gfsequence('CREDIT'))
    lcToSnd  = IIF(MCHOICE = "D" , 'DEBIT' , 'CREDIT')

    *C101632,1 (Begin) Get the sequence no. from CRMEMO in case of "credit" only for
    *C101632,1         the custom program ARKRADC.PRG for 'KRAZI KAT'. Refer to '100820' in 26.
    IF (ASCAN(laEvntTrig,PADR("SEQNO",10)) <> 0)
      = gfDoTriger('ARDBADJ','SEQNO')
    ENDIF  

    *C101632,1 (End)
    lcTranNo = gfsequence(lcToSnd, gcAct_Comp, "", "", "TRAN")
    *-- Update the temp. file with the current info.
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    
    *C101632,1 (Begin)  Replace TRANDATE   WITH ldTranDate in case of debit
    *REPLACE TRAN      WITH lcTranNo ;
      TRANDATE  WITH ldBatchDt ;
      dPostDate WITH ldBatchDt ;
      BATCH     WITH lcBatchNo ;
      cCurrCode WITH lcCurrCode ;
      nExRate   WITH lnExRate ;
      nCurrUnit WITH lnCurrUnit

    *B603418,1 NAD (Start) Not to Replace the tran date and save it as it is in the temp file
    *REPLACE TRAN WITH lcTranNo ;
      TRANDATE   WITH IIF(MCHOICE = 'D' AND llChrgBack ,ldTranDate,ldBatchDt);
      dPostDate  WITH ldBatchDt ;
      BATCH      WITH lcBatchNo ;
      cCurrCode  WITH lcCurrCode ;
      nExRate    WITH lnExRate ;
      nCurrUnit  WITH lnCurrUnit
      
    REPLACE TRAN WITH lcTranNo ;
      dPostDate  WITH ldBatchDt ;
      BATCH      WITH lcBatchNo ;
      cCurrCode  WITH lcCurrCode ;
      nExRate    WITH lnExRate ;
      nCurrUnit  WITH lnCurrUnit  
    *B603418,1 NAD (End)
    *C101632,1 (End)
    
    UNLOCK
    SCATTER MEMVAR MEMO
         
    *C101632,1 (Begin) In case of charge back for Krazi Kat update the charge back date with batch date AND UPDTE NOTES.
    IF (ASCAN(laEvntTrig,PADR("CHRGDATE",10)) <> 0)
      = gfDoTriger('ARDBADJ','CHRGDATE')
    ENDIF  
    *C101632,1 (End)

    *-- Update the master file.
    SELECT (lcCurAlis)
    APPEND BLANK
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    GATHER MEMVAR MEMO    
    *-- Call global function to add audit fields info.
    =gfAdd_Info(lcCurAlis)
    UNLOCK
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 1
    UNLOCK
  ENDIF

  IF &lcAdjTemp..nSteps < 2
    *-- Update the customer file with : open credit, current, total age
    *-- and net balance.
    SELECT CUSTOMER
    IF SEEK('M'+&lcAdjTemp..ACCOUNT)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      *-- Add the equivelant amount to customer file.
      lnEqvAmnt = ROUND(&lcAdjTemp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
      REPLACE OPENCR  WITH IIF(MCHOICE = "D" , OPENCR , OPENCR + lnEqvAmnt) ;
        CURRENT WITH IIF(MCHOICE = "D" , CURRENT + lnEqvAmnt , CURRENT) ;
        TOTAGE  WITH IIF(MCHOICE = "D" , TOTAGE + lnEqvAmnt, TOTAGE) ;
        NETBAL  WITH IIF(MCHOICE = "D" , NETBAL + lnEqvAmnt,NETBAL+(-1 * ABS(lnEqvAmnt)))
      
      *C101632,1 (Begin) Appending the chgback field in case of charge BACK. 
     
      IF MCHOICE = "D" AND UPPER(&lcAdjTemp..CCHRGBACK)='YES'
        REPLACE CHGBACK  WITH CHGBACK+&lcAdjTemp..Amount
      ENDIF
      *C101632,1 (End)
      
      UNLOCK

      *E301245 function to update nHgWtrMark Field with NETBAL field [Begin.]
      =lfHgWUpdat()
      *E301245 function to update nHgWtrMark Field with NETBAL field [End.]
      lcLinkCode = IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF")
      *E500374,5  HBG  08/15/2000 If there is a factor ,take the Link Code of it [Begin]
      IF !EMPTY(&lcAdjTemp..cFacCode)
        lcLinkCode = lfGetFacLk(lcLinkCode)
      ENDIF 
      *E500374,5 [End]
    ELSE
      lcLinkCode = "DEFDEF"
      *E500374,5  HBG  08/15/2000 If there is a factor ,take the Link Code of it [Begin]
      IF !EMPTY(&lcAdjTemp..cFacCode)
        lcLinkCode = lfGetFacLk(lcLinkCode)
      ENDIF 
      *E500374,5 [End]
    ENDIF

    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 2
    UNLOCK
  ELSE
    lcLinkCode = IIF(SEEK('M'+&lcAdjTemp..ACCOUNT , "CUSTOMER") , IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF") , "DEFDEF")
    *E500374,5  HBG  08/15/2000 If there is a factore ,take the Link Code of it [Begin]
    IF !EMPTY(&lcAdjTemp..cFacCode)
      lcLinkCode = lfGetFacLk(lcLinkCode)
    ENDIF 
    *E500374,5 [End]
  ENDIF

  IF &lcAdjTemp..nSteps < 3
    SELECT (lcAdjTemp)
    IF llUpdRep
      lcTran = ALLTRIM('T' + ALLTRIM(STR(RECNO(lcAdjTemp),5)))
      *-- Update the related rep. commissions records with the currency info.
      SELECT (lcRepCBTmp)
      SCAN FOR ALLTRIM(TRAN) = lcTran
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE TRAN      WITH lcTranNo ;
          cCurrCode WITH lcCurrCode ;
          nExRate   WITH lnExRate ;
          nCurrUnit WITH lnCurrUnit ;
          DATE  WITH ldBatchDt ;
          BATCH WITH lcBatchNo
        UNLOCK
      ENDSCAN
    ENDIF
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 3
    UNLOCK
  ENDIF

  IF &lcAdjTemp..nSteps < 4
    IF !EMPTY(lcGLFYear) .AND. !EMPTY(lcGLPeriod) .AND. BETWEEN(VAL(lcGLPeriod) , 1 , 13)
      *E301077,14 Reham On 12/29/98   *** Begin ***
      *E301077,14 Open the factor file.
      =gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
      *E301077,14 Reham On 12/29/98   *** End   ***

      lcGLPeriod = PADL(ALLTRIM(lcGLPeriod) , 2 , "0" )
      *-- Update the customer history file.
      SELECT ARCUSHST
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      IF SEEK(&lcAdjTemp..Account + lcGLFYear , "ARCUSHST")
        IF MCHOICE = "D"
          REPLACE nDrAdj            WITH nDrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
            nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        ELSE
          REPLACE nCrAdj            WITH nCrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
            nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        ENDIF
        UNLOCK
      ENDIF
      *E301077,14 Reham On 12/29/98   *** Begin ***
      =gfCloseFile('ARCUSHST')
      *E301077,14 Reham On 12/29/98   *** End   ***
    ENDIF
    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 4
    UNLOCK
  ENDIF
  IF &lcAdjTemp..nSteps < 5
    *-- Default the Adj. Account with the saved one in the adj. file.
    lcGLAcc = &lcAdjTemp..cAdjAcct

    *-- If there is AP Link, add a record in the AP Payment file.
    IF llLink_AP AND !EMPTY(&lcAdjTemp..cBnkCode) AND !EMPTY(&lcAdjTemp..cChkAcct)
      *-- Add record in the AP payment file.
      SELECT APPAYMNT
      APPEND BLANK
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()

      *E301266,1 AKA (Start)
      *REPLACE cPayType  WITH 'A' ;
      *        cPayMeth  WITH 'A' ;
      *        cBnkCode  WITH &lcAdjTemp..cBnkCode ;
      *        cChkAcct  WITH &lcAdjTemp..cChkAcct ;
      *        dPayDate  WITH ldBatchDt ;
      *        cFisFYear WITH lcGLFYear ;
      *        cFspprdid WITH lcGLPeriod ;
      *        cPayDocNo WITH CUSTOMER.Store ;
      *        cPayClNo  WITH CUSTOMER.Account ;
      *        cPayComp  WITH IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName) ;
      *        nPayAmnt  WITH &lcAdjTemp..Amount ;
      *        cPayRecSt WITH 'O'

      REPLACE cPayType  WITH 'A' ;
        cPayMeth  WITH 'A' ;
        cBnkCode  WITH &lcAdjTemp..cBnkCode ;
        cChkAcct  WITH &lcAdjTemp..cChkAcct ;
        dPayDate  WITH ldBatchDt ;
        cFisFYear WITH lcGLFYear ;
        cFspprdid WITH lcGLPeriod ;
        cPayDocNo WITH CUSTOMER.Store ;
        cPayClNo  WITH CUSTOMER.Account ;
        cPayComp  WITH IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName) ;
        nPayAmnt  WITH &lcAdjTemp..Amount ;
        cPayRecSt WITH 'O'  ;
        Batch     WITH lcBatchNo ;
        cCurrCode WITH lcCurrCode ;
        nExRate   WITH lnExRate ;
        nCurrUnit WITH lnCurrUnit


      *E301266,1 AKA (End)


      *-- Call global function to add audit fields info.
      =gfAdd_Info("APPAYMNT")
      UNLOCK

      *-- Get the GL account from the AP checks file.
      lcGLAcc = IIF(SEEK(&lcAdjTemp..cBnkCode + &lcAdjTemp..cChkAcct , ;
        "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
    ENDIF

    *-- Open GLDIST file to call General Ledger distribution procedure.
    IF llLink_Gl
      *-- Call GL Distribution procedure 2 times for each line of this adj.
      SELECT (lcAdjTemp)
      IF MCHOICE = "C"
        *-- If credit adjustment.
        DO GLDIST WITH 'DEFDEF','009',-(&lcAdjTemp..AMOUNT), ;
          'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
        DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT, ;
          'CA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          '',lcCurrCode,lnCurrUnit,lnExRate
      ELSE
        *-- If debit adjustment.
        DO GLDIST WITH '','010',-(&lcAdjTemp..AMOUNT), ;
          'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          lcGLAcc,lcCurrCode,lnCurrUnit,lnExRate
        DO GLDIST WITH lcLinkCode,'001',&lcAdjTemp..AMOUNT, ;
          'DA',&lcAdjTemp..TRAN,ldBatchDt,lcGLFYear,lcGLPeriod,'&lcGLTemp', ;
          '',lcCurrCode,lnCurrUnit,lnExRate
      ENDIF

      SELECT (lcCurAlis)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
        cAdjAcct WITH lcGLAcc
      UNLOCK

      SELECT (lcAdjTemp)
      *-- Lock the file to grantee the phiscal update.
      =RLOCK()
      REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
        cAdjAcct WITH lcGLAcc
      UNLOCK
    ENDIF

    SELECT (lcAdjTemp)
    *-- Lock the file to grantee the phiscal update.
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
  ENDIF
  
  *E301521,1 (Start) Add new record in the EDITrans table for NC module using
  IF llEdiAccount
    INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) VALUES ;
    ('812',&lcAdjTemp..TranType+&lcAdjTemp..Tran,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)
    =gfAdd_Info('EDITRANS')
  ENDIF
  *E301521,1 (End)
  SELECT (lcAdjTemp)
ENDSCAN
SET ORDER TO TAG (lcAdjTemp)

*-- Append from the temp. GL distribution file to General Ledger
*-- distribution file.
IF llLink_Gl
  *-- Generate a unique session no.
  lcGLSession = gfsequence('GLSESSION')
  SELECT (lcGLTemp)
  REPLACE ALL GLSESSION WITH lcGLSession
  USE
  *-- Update the GL distribution file.
  SELECT GLDIST
  APPEND FROM &gcWorkDir.&lcGLTemp
  =gfOpenFile(gcWorkDir+lcGLTemp , "" ,"EX")
ENDIF

*-- If credit transaction & there is salesreps chargebacks to be updated.
IF llUpdRep .AND. llRepCB
  SELECT (lcRepCBTmp)
  GOTO TOP
  lnCurRec  = 1
  lnTotRec  = RECCOUNT(lcRepCBTmp)
  *-- Scan in the salesreps chargebacks file to update the salesrep & repcomm files.
  SCAN
    *-- Call global function to display thermometer.
    =gfTherm (lnTotRec,lnCurRec,"Updating the salesrep commission File.")
    lnCurRec = lnCurRec + 1
    *-- Calculated the equivalent amount.
    lnCurAmt    = ROUND(&lcRepCBTmp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    lnRpFrnAmnt = &lcRepCBTmp..AMOUNT
    *-- Update the salesrep info if exist in its file.
    SELECT SALESREP
    IF SEEK(&lcRepCBTmp..REPCODE)
      IF &lcRepCBTmp..nSteps < 1
        lnNewBal = SALESREP.BALANCE + lnCurAmt
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE CURRENT WITH CURRENT + lnCurAmt ;
          BALANCE WITH lnNewBal
        UNLOCK

        SELECT (lcRepCBTmp)
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        REPLACE nSteps WITH 1
        UNLOCK
      ENDIF

      SELECT (lcRepCBTmp)
      IF &lcRepCBTmp..nSteps < 2
        *-- Lock the file to grantee the phiscal update.
        =RLOCK()
        *-- Update Repcomm amount with equivalent amount and foreign
        *-- amount with foreign amount.
        REPLACE BALANCE  WITH lnNewBal ;
          Amount   WITH lnCurAmt ;
          nForAmnt WITH lnRpFrnAmnt
        REPLACE nSteps WITH 2
        UNLOCK
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcRepCBTmp)
  USE
  *-- Update the salesreps commesion file.
  SELECT REPCOMM
  APPEND FROM &gcWorkDir.&lcRepCBTmp
  =gfOpenFile(gcWorkDir+lcRepCBTmp , gcWorkDir+lcRepCBTmp ,"EX")
ENDIF

*-- Finish the whole session.
SELECT UNCMSESS
IF lnUnCmSeRc <> 0 .AND. lnUnCmSeRc <= RECCOUNT()
  GOTO lnUnCmSeRc
  REPLACE STATUS WITH 'C'
  llContinue = .F.
  UNLOCK
ENDIF
lnUnCmSeRc = 0
IF !llFromEDI
  *-- Go to select mode.
  laScrMode    = "S"
  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS
ENDIF

*!*************************************************************
*
FUNCTION lfUpdVars

*-- Concatinate the files variable.
lcFiles = 'lcAdjTemp,'+lcAdjTemp+','+lcAdjTemp+';'+ ;
  IIF(MCHOICE="C",'lcRepCBTmp,'+lcRepCBTmp+','+lcRepCBTmp+';','')

*-- Call global function to update the uncompleted session record in the
*-- uncompleted session file with the files names & screen variables.
llNoThing = IIF(lnUnCmSeRc=0, .T., gfSavSess(lcProgID, lcFiles, @laVars, lcSession))

*!*************************************************************************

FUNCTION lfHgWUpdat

=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK
*-- End of lfHgWUpdat.

*!*************************************************************
FUNCTION lfGetFacLk
PARAMETER lclink
PRIVATE lcLnkCod 

IF !llOpenFac
  llOpenFac = gfOpenFile(gcDataDir+'FACTOR',gcDataDir+'FACTOR','SH') 
ENDIF

IF SEEK(&lcAdjTemp..cfaccode,'FACTOR')
  lcLnkCod = IIF(!EMPTY(FACTOR.Link_code) , FACTOR.Link_code , lclink)
ELSE
  lcLnkCod = lclink  
ENDIF


RETURN lcLnkCod 