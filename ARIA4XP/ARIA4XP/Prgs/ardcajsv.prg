*!******************************************************************
*! Program file        : ARDCAJSV
*! Program description : Saving the Debit/Credit Adj. Transcation
*! For screen          : 
*! For System          : Aria Advantage Series - Version 3.0
*! For Module          : Account Receivable - (AR)
*! Developer Name      : Reham Al-Allamy
*! Tracking Job Number : N000429,1
*!******************************************************************
*! Calls               : 
*!******************************************************************
*! Passed Parameters   : None.
*!******************************************************************
*! Example             : DO lfSavDCAdj IN ARDCAJSV.PRG
*!******************************************************************
*! Modifications:
*! E039450,1 MMT 07/13/2005 convert form to use SQL AND FOX data files
*! B607995,1 SSH 03/05/2007 do not Round when update APPAYMNT
*!******************************************************************
FUNCTION lfSavDCAdj
PARAMETERS llFromEdi
 
*-- Open the needed tables for NC module use
llEdiAccount = .F.
lcSysType = gfGetMemVar('M_SYSTYPE',oAriaApplication.ActiveCompanyID)
IF !llFromEdi AND 'NC' $ oAriaApplication.CompanyInstalledModules AND lcSysType = 'P'
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *=gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  *=gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  *=gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
  =gfOpenTable(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenTable(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenTable(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files [End]

  SELECT CODES
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *SET ORDER TO TAG Idrltfname
  gfSetOrder([Idrltfname])
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
  
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *=SEEK('NYCSITEID')
  =gfSeek('NYCSITEID')
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
  LOCATE REST WHILE cdefcode+crltfield+cfld_name = 'NYCSITEID' ;
              FOR   cRltd_Nam = 'CCMSITETYP' AND cRltd_Vlu= 'B'
  IF FOUND()
    lcSiteId = Codes.cCode_No
    SELECT EDiAcPrt

    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *LOCATE FOR cSiteId = lcSiteId
    IF !gfGetRemoteProp('llNative','EDiAcPrt')
      =gfSqlrun([SELECT * from EDiAcPrt where cSiteId =']+lcSiteId +['],'EDiAcPrt')
      LOCATE FOR cSiteId = lcSiteId  && Warning
    ELSE
      LOCATE FOR cSiteId = lcSiteId  && Warning  
    ENDIF 
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]

    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *IF FOUND('EDIACPRT') AND SEEK(EDIACPRT.cPartCode+'812','EDIPD')
    IF FOUND('EDIACPRT') AND gfSeek(EDIACPRT.cPartCode+'812','EDIPD')
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      llEdiAccount = .T.
    ENDIF
  ENDIF
ENDIF

IF llFromEdi
  *-- Check if there is empty account codes in the existing transactions
  SELECT (lcAdjTemp)
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *LOCATE FOR EMPTY(Account) .OR. IIF(MCHOICE = "D" , EMPTY(TranCode) , EMPTY(cCreditCod))
  LOCATE FOR EMPTY(Account) .OR. IIF(MCHOICE = "D" , EMPTY(TranCode) , EMPTY(cCreditCod))  && Warning
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
  IF FOUND()
    GO TOP
    *** There is one or more code(s) is missing.  Cannot update! ***
    *** < Ok > ***
    =gfModalGen("INM40078B00000" , "DIALOG")
    RETURN .F.
  ENDIF
  
  *-- Check if there is any transaction has zero amount.
  lnZero = 0
  COUNT FOR &lcAdjTemp..AMOUNT = 0 TO lnZero
  IF lnZero >= 1
    *** One or more transaction(s) has zero amount. ***
    *** Transaction(s) with zero amount will be ignored. ***
    *** < OK > ***
    =gfModalGen("INM40071B00000" , "DIALOG")
  ENDIF
  
  *-- Get the transactions count to check if there is transactions entered or not.
  lnCount = 0
  COUNT FOR !DELETED() .AND. &lcAdjTemp..AMOUNT <> 0 TO lnCount
  IF lnCount = 0
    *** There is no transactions entered. ***
    *** < OK > ***
    =gfModalGen("INM40083B00000" , "DIALOG")
    RETURN .F.
  ENDIF
  
  *-- Check if the budget amount not equal the actual amount or the transactions count not ecaul to the tape count.
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
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

IF EMPTY(lcBatchNo)
  *-- Get the batch no.
  lcBatchNo = gfsequence('BATCH')
ENDIF

*** Batch No.  : "+lcBatchNo + CHR(13) + "Batch Date : "+DTOC(ldBatchDt) ***
*** < Ok > ***
lcTmpStr = lcBatchNo + "|" + DTOC(ldBatchDt)
=gfModalGen("INM40081B00000" , "DIALOG" , lcTmpStr)

*-- Check if there is salesreps' charge backs or not.
llUpdRep = .F.
IF llRepCB
  SELECT (lcRepCBTmp)
  DELETE ALL FOR AMOUNT = 0
  GOTO TOP
  llUpdRep = IIF(EOF() , .F. , .T.)
ENDIF

*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[START]
lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  RETURN .F.
ENDIF
llUpdate = .F.
*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
*-- update the master credit or debit file with the entered transaction.
SELECT (lcAdjTemp)
SET ORDER TO
GO TOP

lnCurRec = 0
SCAN FOR !DELETED() .AND. AMOUNT <> 0
  SELECT (lcAdjTemp)
  lcToSnd  = IIF(MCHOICE = "D" , 'DEBIT' , 'CREDIT')
  
  *-- Get the sequence no. from CRMEMO in case of "credit" only for
  *-- the custom program ARKRADC.PRG for 'KRAZI KAT'. Refer to '100820' in 26.
  IF (ASCAN(oAriaApplication.laEvntTrig,PADR("SEQNO",10)) <> 0)
    = gfDoTriger('ARDBADJ','SEQNO')
  ENDIF  
  SELECT (lcCurAlis)
  lcTranNo = gfsequence(lcToSnd, oAriaApplication.ActiveCompanyID, "", "", "TRAN")
  *-- Update the temp. file with the currency info.,tran. #, batch # & date
  SELECT (lcAdjTemp)
  
  REPLACE TRAN       WITH lcTranNo ;
          dPostDate  WITH ldBatchDt ;
          BATCH      WITH lcBatchNo ;
          cCurrCode  WITH lcCurrCode ;
          nExRate    WITH lnExRate ;
          nCurrUnit  WITH lnCurrUnit  
  =gfAdd_Info(lcAdjTemp)       
  SCATTER MEMVAR MEMO
       
  *-- In case of charge back for Krazi Kat update the charge back date with batch date AND UPDTE NOTES.
  IF (ASCAN(oAriaApplication.laEvntTrig,PADR("CHRGDATE",10)) <> 0)
    = gfDoTriger('ARDBADJ','CHRGDATE')
  ENDIF  
  
  *-- Update the master file.
  SELECT (lcCurAlis)
  APPEND BLANK
  GATHER MEMVAR MEMO    
  lcCursorUpdate = gfGetRemoteProp('lcCursorUpdate',lcCurAlis)
  IF !EMPTY(lcCursorUpdate) AND !gfGetRemoteProp('llnative',lcCurAlis)
     INSERT INTO (lcCursorUpdate) FROM MEMVAR 
  ENDIF 
  
  *-- Update the customer file with : open credit, current, total age and net balance.
  SELECT CUSTOMER
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *IF SEEK('M'+&lcAdjTemp..ACCOUNT)
  IF gfSeek('M'+&lcAdjTemp..ACCOUNT)
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    *-- Add the equivelant amount to customer file.
    lnEqvAmnt = ROUND(&lcAdjTemp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *REPLACE OPENCR  WITH IIF(MCHOICE = "D" , OPENCR , OPENCR + lnEqvAmnt) ;
            CURRENT WITH IIF(MCHOICE = "D" , CURRENT + lnEqvAmnt , CURRENT) ;
            TOTAGE  WITH IIF(MCHOICE = "D" , TOTAGE + lnEqvAmnt, TOTAGE) ;
            NETBAL  WITH IIF(MCHOICE = "D" , NETBAL + lnEqvAmnt,NETBAL+(-1 * ABS(lnEqvAmnt))) ;
            nHgWtrMark WITH IIF(NETBAL > nHgWtrMark , NETBAL , nHgWtrMark)
    gfReplace([OPENCR  WITH IIF(MCHOICE = "D" , OPENCR , OPENCR + lnEqvAmnt)]+ ;
            [CURRENT WITH IIF(MCHOICE = "D" , CURRENT + lnEqvAmnt , CURRENT)]+ ;
            [TOTAGE  WITH IIF(MCHOICE = "D" , TOTAGE + lnEqvAmnt, TOTAGE)]+ ;
            [NETBAL  WITH IIF(MCHOICE = "D" , NETBAL + lnEqvAmnt,NETBAL+(-1 * ABS(lnEqvAmnt)))]+ ;
            [nHgWtrMark WITH IIF(NETBAL > nHgWtrMark , NETBAL , nHgWtrMark)])
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    
    *-- Append the charge back field in case of "Debit on Account". 
    *IF MCHOICE = "D" AND llChrgBack
    IF MCHOICE = "D" AND &lcAdjTemp..cChrgBack = 'Yes'
      *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
      *REPLACE CHGBACK  WITH CHGBACK+&lcAdjTemp..Amount
      gfReplace([CHGBACK  WITH CHGBACK+&lcAdjTemp..Amount])
      *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    ENDIF
    
    *-- Get the link code value.
    lcLinkCode = IIF(!EMPTY(CUSTOMER.Link_Code) , CUSTOMER.Link_Code , "DEFDEF")
    *-- If there is a factor ,take the Link Code of it.
    IF !EMPTY(&lcAdjTemp..cFacCode)
      lcLinkCode = lfGetFacLk(lcLinkCode)
    ENDIF 
  ELSE
    lcLinkCode = "DEFDEF"
    *-- If there is a factor ,take the Link Code of it
    IF !EMPTY(&lcAdjTemp..cFacCode)
      lcLinkCode = lfGetFacLk(lcLinkCode)
    ENDIF 
  ENDIF

  *-- Update salesrep. info.
  SELECT (lcAdjTemp)
  IF llUpdRep
    
    lcTran = ALLTRIM('T' + ALLTRIM(STR(RECNO(lcAdjTemp),5)))
    *-- Update the related rep. commissions records with the currency info.
    SELECT (lcRepCBTmp)
    SCAN FOR ALLTRIM(TRAN) = lcTran
      REPLACE TRAN      WITH lcTranNo ;
              cCurrCode WITH lcCurrCode ;
              nExRate   WITH lnExRate ;
              nCurrUnit WITH lnCurrUnit ;
              DATE      WITH ldBatchDt ;
              BATCH     WITH lcBatchNo
      
    ENDSCAN
  ENDIF
  
  
  *-- Update the customer history file.
  IF !EMPTY(lcGLFYear) .AND. !EMPTY(lcGLPeriod) .AND. BETWEEN(VAL(lcGLPeriod) , 1 , 13)
    IF !USED("ARCUSHST")
      *E039450,1 MMT [Start]
      *=gfOpenFile(oAriaApplication.DataDir+'ARCUSHST',oAriaApplication.DataDir+'ACTHST','SH')
      =gfOpenTable(oAriaApplication.DataDir+'ARCUSHST',oAriaApplication.DataDir+'ACTHST','SH')
      *E039450,1 MMT [End]
    ENDIF
    lcGLPeriod = PADL(ALLTRIM(lcGLPeriod) , 2 , "0" )
    *-- Update the customer history file.
    SELECT ARCUSHST
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *IF SEEK(&lcAdjTemp..Account + lcGLFYear , "ARCUSHST")
    IF gfSeek(&lcAdjTemp..Account + lcGLFYear , "ARCUSHST")
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      IF MCHOICE = "D"
        *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
        *REPLACE nDrAdj            WITH nDrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
                nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        gfReplace([nDrAdj            WITH nDrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
                nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)])
        *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      ELSE
        *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
        *REPLACE nCrAdj            WITH nCrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
                nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
        gfReplace([nCrAdj            WITH nCrAdj + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2) ;
                nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ROUND(ABS(&lcAdjTemp..Amount) &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)])
        *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      ENDIF
    ENDIF
  ENDIF
  
  *-- Default the Adj. Account with the saved one in the adj. file.
  lcGLAcc = &lcAdjTemp..cAdjAcct
  *-- If there is AP Link, add a record in the AP Payment file.
  IF llLink_AP AND !EMPTY(&lcAdjTemp..cBnkCode) AND !EMPTY(&lcAdjTemp..cChkAcct)
    *-- Add record in the AP payment file.
    SELECT APPAYMNT
    lnCurRec = lnCurRec + 1
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *APPEND BLANK
    gfAppend()
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *REPLACE cPayType  WITH 'A' ;
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
*! B607995,1 SSH 03/05/2007 do not Round when update APPAYMNT
*!*	  gfReplace([cPayType  WITH 'A']+ ;
*!*	            [cPayMeth  WITH 'A'] +;
*!*	            [cBnkCode  WITH ']+&lcAdjTemp..cBnkCode+[' ] +;
*!*	            [cChkAcct  WITH ']+&lcAdjTemp..cChkAcct+[' ] +;
*!*	            [dPayDate  WITH CTOD(']+DTOC(ldBatchDt)+[')]+ ;
*!*	            [cFisFYear WITH ']+lcGLFYear +['] +;
*!*	            [cFspprdid WITH ']+lcGLPeriod+[']+ ;
*!*	            [cPayDocNo WITH ']+CUSTOMER.Store+[' ]+;
*!*	            [cPayClNo  WITH ']+CUSTOMER.Account+[' ]+;
*!*	            [cPayComp  WITH ']+IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName)+[' ] +;
*!*	            [nPayAmnt  WITH VAL(']+ALLTRIM(STR(&lcAdjTemp..Amount))+[') ]+ ;
*!*	            [cPayRecSt WITH 'O' ] +;
*!*	            [Batch     WITH ']+lcBatchNo+['] +;
*!*	            [cCurrCode WITH ']+lcCurrCode+[']+;
*!*	            [nExRate   WITH VAL(']+ALLTRIM(STR(lnExRate))+[') ] +;
*!*	            [nCurrUnit WITH VAL(']+ALLTRIM(STR(lnCurrUnit))+[')])
  

  gfReplace([cPayType  WITH 'A']+ ;
            [cPayMeth  WITH 'A'] +;
            [cBnkCode  WITH ']+&lcAdjTemp..cBnkCode+[' ] +;
            [cChkAcct  WITH ']+&lcAdjTemp..cChkAcct+[' ] +;
            [dPayDate  WITH CTOD(']+DTOC(ldBatchDt)+[')]+ ;
            [cFisFYear WITH ']+lcGLFYear +['] +;
            [cFspprdid WITH ']+lcGLPeriod+[']+ ;
            [cPayDocNo WITH ']+CUSTOMER.Store+[' ]+;
            [cPayClNo  WITH ']+CUSTOMER.Account+[' ]+;
            [cPayComp  WITH ']+IIF(!EMPTY(&lcAdjTemp..REFERENCE),&lcAdjTemp..REFERENCE,CUSTOMER.BtName)+[' ] +;
            [nPayAmnt  WITH VAL(']+ALLTRIM(STR(&lcAdjTemp..Amount,14,2))+[') ]+ ;
            [cPayRecSt WITH 'O' ] +;
            [Batch     WITH ']+lcBatchNo+['] +;
            [cCurrCode WITH ']+lcCurrCode+[']+;
            [nExRate   WITH VAL(']+ALLTRIM(STR(lnExRate,9,4))+[') ] +;
            [nCurrUnit WITH VAL(']+ALLTRIM(STR(lnCurrUnit))+[')])

*! B607995,1 SSH 03/05/2007 do not Round when update APPAYMNT
  IF !gfGetRemoteProp('llNative','APPAYMNT')
    gfReplace([LineNo WITH lnCurRec ])
  ENDIF 

    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    =gfAdd_Info("APPAYMNT")
    lcCursorUpdate = gfGetRemoteProp('lcCursorUpdate','APPAYMNT')
    IF !EMPTY(lcCursorUpdate) AND !gfGetRemoteProp('llNative','APPAYMNT')
      SELECT(lcCursorUpdate)
      =gfAdd_Info(lcCursorUpdate)
      SELECT('APPAYMNT')
    ENDIF 
  *  =gfTableUpdate(.F.,"APPAYMNT")
    *-- Get the GL account from the AP checks file.
    *E039450,1 MMT [Start]
    *lcGLAcc = IIF(SEEK(&lcAdjTemp..cBnkCode + &lcAdjTemp..cChkAcct , "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
    lcGLAcc = IIF(gfSeek(&lcAdjTemp..cBnkCode + &lcAdjTemp..cChkAcct , "APChecks") , APChecks.cChkGLAcc , lcGLAcc)
    *E039450,1 MMT [End]
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
    
    *-- Update the GL accounts in both master & temp. file.
    SELECT (lcCurAlis)
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
            cAdjAcct WITH lcGLAcc
    gfReplace([cArGLAcc WITH &lcGLTemp..GLAccount ;
            cAdjAcct WITH lcGLAcc])
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
    SELECT (lcAdjTemp)
    REPLACE cArGLAcc WITH &lcGLTemp..GLAccount ;
            cAdjAcct WITH lcGLAcc
    
  ENDIF

  *-- Add new record in the EDITrans table for NC module using
  IF llEdiAccount
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) VALUES ;
               ('812',&lcAdjTemp..TranType+&lcAdjTemp..Tran,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)
    INSERT INTO 'EDITRANS' (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) VALUES ;
               ('812',&lcAdjTemp..TranType+&lcAdjTemp..Tran,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)  && Warning
    =gfAdd_Info('EDITRANS')
    lcCursorUpdate = gfGetRemoteProp('lcCursorUpdate','EDITRANS')
    IF !EMPTY(lcCursorUpdate) AND !gfGetRemoteProp('llNative','EDITRANS')
      INSERT INTO (lcCursorUpdate) (cEdiTrnTyp,Key,Type,cPartner,cStatus,lInterComp) VALUES ;
               ('812',&lcAdjTemp..TranType+&lcAdjTemp..Tran,'A',EdiAcPrt.cPartner,'N',EdiAcPrt.lInterComp)  && Warning
      =gfAdd_Info(lcCursorUpdate)         
    ENDIF 
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]

  ENDIF
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[START]
  llUpdate = gfTableUpdate(lcTranCode ,lcCurAlis)
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[END]
  SELECT (lcAdjTemp)
ENDSCAN
*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[START]
IF !llUpdate
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  RETURN .F.
ELSE
    =oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
ENDIF
*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[END]
SELECT (lcAdjTemp)
SET ORDER TO TAG (lcAdjTemp)

*-- Append from the temp. GL distribution file to General Ledger distribution file.
IF llLink_Gl
  *-- Generate a unique session no.
  lcGLSession = gfsequence('GLSESSION')
  SELECT (lcGLTemp)
  REPLACE ALL GLSESSION WITH lcGLSession
  USE
  
  *-- Update the GL distribution file.
  SELECT GLDIST
  lcTempGLDist = oAriaApplication.WorkDir + lcGLTemp
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *APPEND FROM &lcTempGLDist
  APPEND FROM &lcTempGLDist  
  lcCursorUpdate = gfGetRemoteProp('lcCursorUpdate','GLDIST')
  IF !EMPTY(lcCursorUpdate) AND !gfGetRemoteProp('llNative','GLDIST')
    SELECT(lcCursorUpdate)
    APPEND FROM &lcTempGLDist  
  ENDIF
  SELECT GLDIST
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
  =gfOpenFile(oAriaApplication.WorkDir+lcGLTemp , "" ,"EX")
  
ENDIF

*-- If credit transaction & there is salesreps chargebacks to be updated.
IF llUpdRep .AND. llRepCB
  SELECT (lcRepCBTmp)
  GOTO TOP
  *-- Scan in the salesreps chargebacks file to update the salesrep & repcomm files.
  SCAN
    *-- Calculated the equivalent amount.
    lnCurAmt    = ROUND(&lcRepCBTmp..AMOUNT &lcExRSin lnExRate &lcUntSin lnCurrUnit,2)
    lnRpFrnAmnt = &lcRepCBTmp..AMOUNT
    *-- Update the salesrep info if exist in its file.
    SELECT SALESREP
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
    *IF SEEK(&lcRepCBTmp..REPCODE)
    IF gfSeek(&lcRepCBTmp..REPCODE)
    *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      lnNewBal = SALESREP.BALANCE + lnCurAmt
      *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
      *REPLACE CURRENT WITH CURRENT + lnCurAmt ;
              BALANCE WITH lnNewBal
      gfReplace([CURRENT WITH CURRENT + lnCurAmt ;
              BALANCE WITH lnNewBal])
      *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
      SELECT (lcRepCBTmp)
      *-- Update Repcomm amount with equivalent amount and foreign amount with foreign amount.
      REPLACE BALANCE  WITH lnNewBal ;
              Amount   WITH lnCurAmt ;
              nForAmnt WITH lnRpFrnAmnt
      
      =gfAdd_Info(lcRepCBTmp)
    ENDIF
  ENDSCAN
  SELECT (lcRepCBTmp)
  USE
  *-- Update the salesreps commession file.
  SELECT REPCOMM
  lcRepTemp = oAriaApplication.WorkDir + lcRepCBTmp
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *APPEND FROM &lcRepTemp
  APPEND FROM &lcRepTemp  
  lcCursorUpdate = gfGetRemoteProp('lcCursorUpdate','REPCOMM')
  IF !EMPTY(lcCursorUpdate) AND !gfGetRemoteProp('llNative','REPCOMM')
    SELECT(lcCursorUpdate)
    APPEND FROM &lcRepTemp  && Warning
  ENDIF
  SELECT REPCOMM
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
   =gfOpenFile(oAriaApplication.WorkDir+lcRepCBTmp , oAriaApplication.WorkDir+lcRepCBTmp ,"EX")
ENDIF

*:******************************************************************
*! PROG      : lfGetFacLk
*! Developer : Reham Al-Allamy
*! Date      : 08/26/2002
*! DESC      : Function to return the link code of the factor
*:******************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : None
*!******************************************************************
*! Passed Parameters  : lcLink -> Link code
*!******************************************************************
*! Returns   : None
*!******************************************************************
*! Example   : None
*!******************************************************************
FUNCTION lfGetFacLk
PARAMETER lclink
PRIVATE lcLnkCod 

*-- Open the factor file.
IF !USED("FACTOR")
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
  *=gfOpenFile(oAriaApplication.DataDir+'FACTOR',oAriaApplication.DataDir+'FACTOR','SH') 
  =gfOpenTable(oAriaApplication.DataDir+'FACTOR',oAriaApplication.DataDir+'FACTOR','SH') 
  *E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
ENDIF

*-- Get the link code for the current factor code.
*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[Start]
*IF SEEK(&lcAdjTemp..cfaccode,'FACTOR')
IF gfSeek(&lcAdjTemp..cfaccode,'FACTOR')
*E039450,1 MMT 07/13/2005 Convert Screen to work with both SQL and FOX files[End]
  lcLnkCod = IIF(!EMPTY(FACTOR.Link_code) , FACTOR.Link_code , lclink)
ELSE
  lcLnkCod = lclink
ENDIF

RETURN lcLnkCod 
