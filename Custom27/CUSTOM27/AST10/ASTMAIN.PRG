*!**************************************************************************
*! Name      : ASTMAIN.PRG
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : A.S.T Sportwear Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfwRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : When for Sales rep code in both Sales Order & Invoice screen
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Example   : =gfUsrVldFn('lfwRep3','AST10')
*!**************************************************************************
*
FUNCTION lfwRep3
PARAMETERS lcRetVal
lcRetVal = .T.
PRIVATE lcOldValue , lcFileName
DO CASE
  CASE SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" ;
       OR SUBSTR(gcbasewind,4) = "SOCONT" 
    lcFileName = lcOrdHdr
  CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"
    lcFileName = lcInvHdr
    
    *-- Note we're in Add mode laScrMode[4] but when opening User defined 
    *-- option grid, it's changed to Edit mode laScrMode[3].
    IF SUBSTR(gcbasewind,4) = "ARIINV"
      PRIVATE lcMainAcct , lnCustRecn
      lnCustRecn = RECNO('Customer')
      lcMainAcct = IIF(SEEK('M'+Account,'Customer'),Customer.Consol,'')    
      IF laScrMode[3] AND laSetups[2,2]<>'Y' AND IIF(lcMainAcct='Y',&lcInvHdr..CONSOL='Y',.T.)
        lcRetVal = .T.
      ELSE
        lcRetVal = .F.
      ENDIF
      IF BETWEEN(lnCustRecn,1,RECCOUNT('Customer'))
        GOTO lnCustRecn IN Customer
      ENDIF
    ELSE
      IF EMPTY(lcInvStat)
        lcRetVal = IIF(laScrMode[3] AND laSetups[2,2]<>'Y',.T.,.F.)
      ELSE
        lcRetVal = .F.
      ENDIF      
    ENDIF  
  CASE SUBSTR(gcbasewind,4) = "RMCRMEM"
    lcFileName = lcCrMemHdr
ENDCASE
IF FILE('&gcWorkDir.'+lcFileName+'.MEM')
  lcOldValue = EVALUATE(SYS(18))
ELSE  
  lcOldValue = SPACE(3)  
ENDIF  
SAVE ALL LIKE lcOldValue TO (gcWorkDir+lcFileName)
RETURN  lcRetVal
*-- End of lfwRep3

*!**************************************************************************
*! Name      : lfvRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Validate Sales rep code in both Sales Order & Invoice screen
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Example   : =gfUsrVldFn('lfvRep3','AST10')
*!**************************************************************************
*
FUNCTION lfvRep3
PARAMETERS lcRetVal
lcRetVal = .T.
IF SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" ;
  OR SUBSTR(gcbasewind,4) = "SOCONT" OR SUBSTR(gcbasewind,4) = "ARDINV" ;
  OR SUBSTR(gcbasewind,4) = "ARIINV" OR SUBSTR(gcbasewind,4) = "RMCRMEM"
  DO lpBrowRep3
ENDIF  
RETURN  lcRetVal
*-- End of lfvRep3

*!**************************************************************************
*! Name      : lpBrowRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Custom Validation of Rep3 for A.S.T. (Sales Order screen).
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Validation of User defined fields from SydField.
*!**************************************************************************
*! Example   : DO lpBrowRep3
*!**************************************************************************
*
PROCEDURE lpBrowRep3
PRIVATE lnRep3Comm
*-- If laUsrField is defined.
IF TYPE('LAUSRFIELD') <> "U"
  PRIVATE lnRep3Pos , lcVar , lcObj
  lnRep3Pos = ASCAN(laUsrField,"REP3")
  IF lnRep3Pos > 0
    lnRep3Pos = ASUBSCRIPT(laUsrField,lnRep3Pos,1)
  ENDIF

  lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
  *-- Empty means that user pressed Reset and cancel buttons respectively
  *IF EMPTY(laOGFxFlt[lnRep3Pos,6])
  *  lcObj = laUsrField[lnRep3Pos,6]
  *ELSE
    lcObj = EVALUATE(SYS(18))      && Variable to hold the current field value
  *ENDIF
  
  IF lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnRep3Pos)) + ",6)"
    PRIVATE lcFileName , lcRepComm
    DO CASE
      CASE SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" OR SUBSTR(gcbasewind,4) = "SOCONT" 
        lcFileName = lcOrdHdr
        lcRepComm = laSetups[11,2]
      CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"
        lcFileName = lcInvHdr
        lcRepComm = laSetups[14,2]
      CASE SUBSTR(gcbasewind,4) = "RMCRMEM"
        lcFileName = lcCrMemHdr
        lcRepComm = 'C'    && Customer only 
    ENDCASE
    PRIVATE llSalesRep
    llSalesRep = gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
    IF !EMPTY(lcObj) AND !SEEK(lcObj,'SalesRep')
      SELECT SalesRep
      DO REPCHK WITH lcObj

      IF EMPTY(lcObj)
        PRIVATE lcOldValue
        lcOldValue = '' 

        IF FILE('&gcWorkDir.'+lcFileName+'.MEM')
          RESTORE ADDITIVE FROM (gcWorkDir+lcFileName)
          lcObj = ALLTRIM(lcOldValue)
        ENDIF  
      ENDIF    
    ENDIF
    SELECT (lcFileName)
    lnRep3Comm = 0
    IF Rep3 <> lcObj
      *-- Get commission in case of Sales Order and Invoice as it's not done in Return merchandise.
      PRIVATE llRepComm
      llRepComm = .F.
      IF lcRepComm = 'D'
        IF lcRepComm = 'D'
          llRepComm = gfOpenFile(gcDataDir+'REP_DIV',gcDataDir+'REP_DIV','SH')
        ENDIF
        *-- Division = laData[15] in Sales Order screen, laData[13] in Direct Invoice , Invoice sales order
        IF SEEK(lcObj+IIF(TYPE('lcOrdHdr') <> 'U',laData[15],laData[13]),'REP_DIV')
          lnRep3Comm = REP_DIV.Comm_Rate
        ELSE
          lnRep3Comm = IIF(EMPTY(lcObj),0,SalesRep.Comm)
        ENDIF
      ELSE
        lnRep3Comm = IIF(EMPTY(lcObj),0,SalesRep.Comm)
      ENDIF
      IF TYPE('lcOrdHdr') <> 'U' AND lcFileName = lcOrdHdr
        SELECT (lcOrdLine)
        lnRecno = RECNO()
        IF SEEK(&lcOrdHdr..CordType+&lcOrdHdr..Order)
          SCAN WHILE CordType+Order+STR(LineNo,6) = &lcOrdHdr..CordType+&lcOrdHdr..Order
            M.Comm3 = lnRep3Comm
            =lfVComm('3')
          ENDSCAN  
        ENDIF  
        SELECT (lcOrdLine)
        GO TOP
        IF !EOF(lcOrdLine)
          GO lnRecNo
        ENDIF
      ENDIF  
      IF llRepComm
        USE IN Rep_Div
      ENDIF
      SELECT (lcFileName)
      =RLOCK()
      REPLACE Rep3  WITH lcObj ,;
              Comm3 WITH lnRep3Comm
      
      DO lpUpdComAm
      UNLOCK
      llCUpdate = .T.

      *-- Update Comm3 every time even if it's zero to overwrite any previous sales rep commission.
      PRIVATE lnComm3Pos
      lnComm3Pos = ASCAN(laUsrField,"COMM3")
      IF lnComm3Pos > 0
        lnComm3Pos = ASUBSCRIPT(laUsrField,lnComm3Pos,1)
        laUsrField[lnComm3Pos,6] = lnRep3Comm
        IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
          laOGFxFlt[lnComm3Pos,6] = lnRep3Comm
        ENDIF  
      ENDIF
    ENDIF
    IF llSalesRep
      USE IN SalesRep
    ENDIF
    &lcVar = lcObj
    laUsrField[lnRep3Pos,6] = lcObj
    IF TYPE('laOGFxFlt[lnRep3Pos,6]') <> 'U'
      laOGFxFlt[lnRep3Pos,6] = lcObj
    ENDIF
  ENDIF
ENDIF        && laUsrField is defined.
*--End of lpBrowRep3. 

*!**************************************************************************
*! Name      : lfwComm3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : When function for Commissions in both Sales Order & Invoice screen
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Example   : =gfUsrVldFn('lfwComm3','AST10')
*!**************************************************************************
*
FUNCTION lfwComm3
PARAMETERS lcRetVal
lcRetVal = .T.
DO CASE
  CASE SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" ;
       OR SUBSTR(gcbasewind,4) = "SOCONT"       && it's left with no code.
  CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"
    *-- Note we're in Add mode laScrMode[4] but when opening User defined 
    *-- option grid, it's changed to Edit mode laScrMode[3].
    IF SUBSTR(gcbasewind,4) = "ARIINV"
      PRIVATE lcMainAcct , lnCustRecn
      lnCustRecn = RECNO('Customer')
      lcMainAcct = IIF(SEEK('M'+Account,'Customer'),Customer.Consol,'')    
      IF laScrMode[3] AND laSetups[2,2]<>'Y' AND IIF(lcMainAcct='Y',&lcInvHdr..CONSOL='Y',.T.)
        lcRetVal = .T.
      ELSE
       lcRetVal = .F.
      ENDIF
      IF BETWEEN(lnCustRecn,1,RECCOUNT('Customer'))
        GOTO lnCustRecn IN Customer
      ENDIF
    ELSE
      IF EMPTY(lcInvStat)
        lcRetVal = IIF(laScrMode[3] AND laSetups[2,2]<>'Y',.T.,.F.)
      ELSE
        lcRetVal = .F.
      ENDIF  
    ENDIF  
  CASE SUBSTR(gcbasewind,4) = "RMCRMEM"       && it's left with no code.
ENDCASE
RETURN  lcRetVal
*-- End of lfwComm3

*!**************************************************************************
*! Name      : lfvComm3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Validate Commissions in both Sales Order & Invoice screen
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Example   : =gfUsrVldFn('lfvComm3','AST10')
*!**************************************************************************
*
FUNCTION lfvComm3
PARAMETERS lcRetVal
lcRetVal = .T.
DO CASE
  CASE SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" ;
       OR SUBSTR(gcbasewind,4) = "SOCONT" 
    DO lpComm3 WITH (lcOrdHdr)
  CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"
    DO lpComm3 WITH (lcInvHdr)
  CASE SUBSTR(gcbasewind,4) = "RMCRMEM"
    DO lpComm3 WITH (lcCrMemHdr)
ENDCASE
RETURN  lcRetVal
*-- End of lfvComm3

*!**************************************************************************
*! Name      : lpComm3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Custom Validation of Comm3 for A.S.T. (Sales Order screen).
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Validation of User defined fields from SydField.
*!**************************************************************************
*! Example   : DO lpComm3
*!**************************************************************************
*
PROCEDURE lpComm3
PARAMETER lcFileName
PRIVATE lcAlias
lcAlias = ALIAS()
IF TYPE('LAUSRFIELD') <> "U"
  PRIVATE lnComm3Pos , lcVar , lcObj
  lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
  lcObj = EVALUATE(SYS(18))      && Variable to hold the current field value

  lnComm3Pos = ASCAN(laUsrField,"COMM3")
  IF lnComm3Pos > 0
    lnComm3Pos = ASUBSCRIPT(laUsrField,lnComm3Pos,1)
  ENDIF
  
  IF lcVar == "LAOGFXFLT(" + ALLTRIM(STR(lnComm3Pos)) + ",6)"
    SELECT (lcFileName)
    IF COMM3 <> lcObj
      =RLOCK()
      REPLACE Comm3 WITH lcObj
      UNLOCK
      llCUpdate = .T.
      &lcVar = lcObj
      laUsrField[lnComm3Pos,6] = lcObj      
      IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
        laOGFxFlt[lnComm3Pos,6] = lcObj
      ENDIF
      DO lpUpdComAm
    ENDIF
  ENDIF  
ENDIF        && laUsrField is defined.
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF  
*-- End of lpOrdComm3.

*!**************************************************************************
*! Name      : lfEraseMem
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Erase Mem file which is used to save the old value in it.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Sales Order program (SOORD).
*!**************************************************************************
*! Example   : =gfDoTriger('SOORD',PADR('ERASEMEM',10))
*!**************************************************************************
*
FUNCTION lfEraseMem
ERASE (gcWorkDir+lcOrdHdr+".MEM")
*-- End of lfEraseMem.

*!**************************************************************************
*! Name      : lfCalcRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Calculate Sales Rep 3 for invoices.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice Sales Order or Direct invoice.
*!**************************************************************************
*! Example   : =gfDoTriger('ARIINV',PADR('CALCREP3',10))
*!**************************************************************************
*
FUNCTION lfCalcRep3
*IF m.Consol<>'Y' .AND. &lcHdrFile..nSteps < 3 .AND. SEEK(m.Rep3,'SALESREP')
IF m.Consol<>'Y' .AND. &lcHdrFile..nSteps < 4 .AND. SEEK(m.Rep3,'SALESREP')
  SELECT SALESREP
  lnEqvAmnt = ROUND(m.COMMAMT3 &lcExRSin m.nExRate &lcUntSin m.nCurrUnit,2)      
  IF lnEqvAmnt > 0
    =RLOCK()
    REPLACE CURRENT WITH CURRENT + lnEqvAmnt ,;
            BALANCE WITH BALANCE + lnEqvAmnt
    UNLOCK
    SELECT REPCOMM
    APPEND BLANK
    =RLOCK()
    REPLACE REPCODE   WITH m.Rep3      ,;
            TRANTYPE  WITH '1'         ,;
            STATUS    WITH 'O'         ,;
            TRAN      WITH m.Invoice   ,;
            ACCOUNT   WITH m.Account   ,;
            ORDER     WITH m.Order     ,;
            CUSTPO    WITH m.CustPo    ,;
            STORE     WITH m.Store     ,;
            DATE      WITH m.InvDate   ,;
            DESC      WITH 'INVOICE'   ,;
            AMOUNT    WITH lnEqvAmnt   ,;
            COMMPCNT  WITH m.Comm3     ,;
            nForAmnt  WITH m.CommAmt3  ,;
            cCurrCode WITH m.cCurrCode ,;
            nCurrUnit WITH m.nCurrUnit ,;
            nExRate   WITH m.nExRate   ,;
            dAdd_Date WITH gdSysDate   ,;
            cAdd_Time WITH TIME()      ,;
            cAdd_User WITH gcUser_id
    UNLOCK
  ENDIF
  
  *SELECT (lcHdrFile)
  *=RLOCK()
  *REPLACE nSteps WITH 3
  *UNLOCK
ENDIF
*-- End of lfCalcRep3.

*!**************************************************************************
*! Name      : lfVoidRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Calculate voiding of Sales Rep 3 for invoices.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Direct invoice.
*!**************************************************************************
*! Example   : =gfDoTriger('ARDINV',PADR('VOIDREP3',10))
*!**************************************************************************
*
FUNCTION lfVoidRep3
IF (!EMPTY(&lcInvHFile..Rep3) .AND. &lcInvHFile..CommAmt3 > 0) .AND. ;
  SEEK(&lcInvHFile..Rep3,'SALESREP')
  WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
  lnEqvAmnt = -1 * &lcInvHFile..CommAmt3 &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
  IF &lcInvHdr..nSteps < 7
    SELECT REPCOMM
    APPEND BLANK
    =RLOCK()
    REPLACE STATUS     WITH 'O'           ,;
            REPCODE    WITH &lcInvHFile..REP3   ,;
            ORDER      WITH &lcInvHFile..Order  ,;
            TRAN       WITH &lcInvHFile..Invoice,;
            DATE       WITH ldVDate     ,;
            TRANTYPE   WITH '6'           ,;
            DESC       WITH 'CHARGE BACK - VOID',;
            CUSTPO     WITH &lcInvHFile..CustPo ,;
            ACCOUNT    WITH &lcInvHFile..Account,;
            STORE      WITH &lcInvHFile..Store  ,;
            AMOUNT     WITH lnEqvAmnt     ,;
            BALANCE    WITH SALESREP.BALANCE + lnEqvAmnt,;
            nForAmnt   WITH -1 *  &lcInvHFile..COMMAMT3 ,;
            cCurrCode  WITH InvHdr.cCurrCode,;
            nCurrUnit  WITH InvHdr.nCurrUnit,;
            nExRate    WITH InvHdr.nExRate  ,;
            dAdd_Date  WITH gdSysDate  ,;
            cAdd_Time  WITH TIME()  ,;
            cAdd_User  WITH gcUser_id
    UNLOCK
  ENDIF
  SELECT SALESREP
  =RLOCK()
  REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
          BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
  UNLOCK
ENDIF  
*-- End of lfVoidRep3.

*!**************************************************************************
*! Name      : lfAddRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Add 3rd Sales Rep from OrdHdr into InvHdr.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice Sales Order.
*!**************************************************************************
*! Example   : =gfDoTriger('ARIINV',PADR('ADDREP3',10))
*!**************************************************************************
*
FUNCTION lfAddRep3
REPLACE Rep3     WITH OrdHdr.Rep3  ,;
        Comm3    WITH OrdHdr.Comm3 ,;
        CommAmt3 WITH (ShipAmt* (1-DiscPcnt/100)*(1-Trde_Disc/100)) * OrdHdr.Comm3/100
*-- End of lfAddRep3.

*!**************************************************************************
*! Name      : lfUserFlds
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Update user define fields in Invoice Sales Order screen.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice Sales Order screen.
*!**************************************************************************
*! Example   : =gfDoTriger('ARIINV',PADR('USERFLDS',10))
*!**************************************************************************
*
FUNCTION lfUserFlds
PRIVATE lnRep3Pos , lnComm3Pos , lnComAm3Po
lnRep3Pos = ASCAN(laUsrField,"REP3")
IF lnRep3Pos > 0
  lnRep3Pos = ASUBSCRIPT(laUsrField,lnRep3Pos,1)
  laUsrField[lnRep3Pos,6] = &lcInvHdr..Rep3
  IF TYPE('laOGFxFlt[lnRep3Pos,6]') <> 'U'
    laOGFxFlt[lnRep3Pos,6] = &lcInvHdr..Rep3
  ENDIF  
ENDIF

lnComm3Pos = ASCAN(laUsrField,"COMM3")
IF lnComm3Pos > 0
  lnComm3Pos = ASUBSCRIPT(laUsrField,lnComm3Pos,1)
  laUsrField[lnComm3Pos,6] = &lcInvHdr..Comm3
  IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
    laOGFxFlt[lnComm3Pos,6] = &lcInvHdr..Comm3
  ENDIF  
ENDIF

lnComAm3Po = ASCAN(laUsrField,"COMMAMT3")
IF lnComAm3Po > 0
  lnComAm3Po = ASUBSCRIPT(laUsrField,lnComAm3Po,1)
  laUsrField[lnComAm3Po,6] = &lcInvHdr..CommAmt3
  IF TYPE('laOGFxFlt[lnComAm3Po,6]') <> 'U'
    laOGFxFlt[lnComAm3Po,6] = &lcInvHdr..CommAmt3
  ENDIF  
ENDIF
*-- End of lfUserFlds

*!**************************************************************************
*! Name      : lfwComPct3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : When function for Comm. Percent in both Sales Order & Invoice screen
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*This function is not used anymore.
FUNCTION lfwComPct3
PARAMETERS lcRetVal
lcRetVal = .T.
DO CASE
  CASE SUBSTR(gcbasewind,4) = "SOORD" OR SUBSTR(gcbasewind,4) = "SOEDORD" ;
       OR SUBSTR(gcbasewind,4) = "SOCONT"       && it's left with no code.
  CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"

    *-- Note we're in Add mode laScrMode[4] but when opening User defined 
    *-- option grid, it's changed to Edit mode laScrMode[3].
    IF SUBSTR(gcbasewind,4) = "ARIINV"
      PRIVATE lcMainAcct , lnCustRecn
      lnCustRecn = RECNO('Customer')
      lcMainAcct = IIF(SEEK('M'+Account,'Customer'),Customer.Consol,'')    
      IF laScrMode[3] AND laSetups[2,2]<>'Y' AND IIF(lcMainAcct='Y',&lcInvHdr..CONSOL='Y',.T.)
        lcRetVal = .T.
      ELSE
        lcRetVal = .F.
      ENDIF
      IF BETWEEN(lnCustRecn,1,RECCOUNT('Customer'))
        GOTO lnCustRecn IN Customer
      ENDIF
    ELSE
      lcRetVal = IIF(laScrMode[3] AND laSetups[2,2]<>'Y',.T.,.F.)
    ENDIF  
ENDCASE
RETURN  lcRetVal
*-- End of lfwComPct3

*!**************************************************************************
*! Name      : lfMemoRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Calculate Sales Rep 3 for Return .
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Credit Memo screen.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('MEMOREP3',10))
*!**************************************************************************
*
FUNCTION lfMemoRep3
IF &lcCrMemHdr..nSteps < 9 AND !EMPTY(&lcCrMemHdr..Rep3) .AND. &lcCrMemHdr..Comm3 > 0
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  
  *-- Update [COMMISSIONS] Salesreps commissions / chargebacks
  SELECT SALESREP
  IF SEEK(&lcCrMemHdr..Rep3 , "SALESREP")
    *-- Calculated the equivalent amount.
    lnFrnAmnt = &lcCrMemHdr..CommAmt3          
    lnCommDue = ROUND(&lcCrMemHdr..CommAmt3 &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit , 2)
    lnNewBal = SALESREP.BALANCE  + lnCommDue
    SELECT SALESREP
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE CURRENT WITH CURRENT + lnCommDue ;
            BALANCE WITH lnNewBal
    UNLOCK
    IF &lcCrMemHdr..TotCredit > 0
      =gfOpenFile(gcDataDir+'REPCOMM',gcDataDir+'REPCOMM','SH')
      SELECT REPCOMM
      APPEND BLANK
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE STATUS     WITH 'O' ;
              REPCODE    WITH &lcCrMemHdr..Rep3 ;
              ACCOUNT    WITH &lcCrMemHdr..Account ;
              ORDER      WITH &lcCrMemHdr..Order ;
              DATE       WITH &lcCrMemHdr..CrDate ;
              TRANTYPE   WITH '5' ;
              DESC       WITH 'RETURN/I#'+&lcCrMemHdr..Invoice ;
              TRAN       WITH &lcCrMemHdr..CrMemo ;
              CUSTPO     WITH &lcCrMemHdr..CustPo ;
              COMMPCNT   WITH &lcCrMemHdr..Comm3 ;
              AMOUNT     WITH lnCommDue ;
              BALANCE    WITH lnNewBal ;
              cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
              nExRate    WITH &lcCrMemHdr..nExRate ;
              nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
              nForAmnt   WITH &lcCrMemHdr..CommAmt3                    
            
      *-- Call global function to add audit fields info.
      =gfAdd_Info("REPCOMM")
      UNLOCK            
      =gfCloseFile('REPCOMM')
    ENDIF
  ENDIF
  =gfCloseFile('SalesRep')
ENDIF
*-- End of lfMemoRep3.

*!**************************************************************************
*! Name      : lfVMemRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Void Sales Rep 3 for Return .
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Credit Memo screen.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('VMEMREP3',10))
*!**************************************************************************
*
FUNCTION lfVMemRep3
*-- [COMMISSIONS] Salesreps commissions / chargebacks.
IF &lcCrMemHdr..nSteps < 5 AND RETHDR.COMMAMT3 <> 0   
  SELECT RETHDR
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  IF !EMPTY(RETHDR.REP3) .AND. RETHDR.COMM3 > 0          
    lnCommDue = ABS(RETHDR.COMMAMT3)          
    *-- Update commissions for salesreps.
    SELECT SALESREP
    IF SEEK(RETHDR.REP3)          
      *-- Calculated the equivalent amount.
      lnFrnAmnt = lnCommDue
      lnCommDue  = ROUND(lnCommDue &lcExRSin laData[33] &lcUntSin laData[32],2)            
      lnNewBal = SALESREP.BALANCE + lnCommDue
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnCommDue ;
              BALANCE  WITH lnNewBal
      UNLOCK
      *-- Call Global Function to transmit the local data.
      IF RETHDR.TOTCREDIT <> 0
        =gfOpenFile(gcDataDir+'REPCOMM',gcDataDir+'REPCOMM','SH')
        SELECT REPCOMM
        APPEND BLANK
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        REPLACE STATUS     WITH 'O' ;
                REPCODE    WITH RETHDR.REP3 ;
                ACCOUNT    WITH RETHDR.ACCOUNT ;
                ORDER      WITH RETHDR.ORDER ;
                DATE       WITH RETHDR.CrDate ;
                TRANTYPE   WITH '7' ;
                DESC       WITH 'C/B-VOID CR/ME' ;
                TRAN       WITH RETHDR.CRMEMO ;
                CUSTPO     WITH RETHDR.CUSTPO ;
                COMMPCNT   WITH RETHDR.Comm3 ;
                AMOUNT     WITH lnCommDue ;
                BALANCE    WITH lnNewBal ;
                cCurrCode  WITH RETHDR.cCurrCode ;
                nExRate    WITH RETHDR.nExRate ;
                nCurrUnit  WITH RETHDR.nCurrUnit ;
                nForAmnt   WITH lnFrnAmnt
        *-- Call global function to add audit fields info.
        =gfAdd_Info("REPCOMM")
        UNLOCK
        =gfCloseFile('REPCOMM')
      ENDIF
    ENDIF
  ENDIF
ENDIF
*-- End of lfVMemRep3.

*!**************************************************************************
*! Name      : lfGetRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Get Sales Rep 3 for Return from Invoice.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Credit Memo screen.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('GETREP3',10))
*!**************************************************************************
*
FUNCTION lfGetRep3
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcCrMemHdr)
REPLACE Rep3  WITH InvHdr.Rep3  , ;   
        Comm3 WITH InvHdr.Comm3 , ;
        CommAmt3 WITH InvHdr.CommAmt3 * -1

PRIVATE lnRep3Pos , lnComm3Pos , lnComAm3Po
lnRep3Pos = ASCAN(laUsrField,"REP3")
IF lnRep3Pos > 0
  lnRep3Pos = ASUBSCRIPT(laUsrField,lnRep3Pos,1)
  laUsrField[lnRep3Pos,6] = &lcCrMemHdr..Rep3
  IF TYPE('laOGFxFlt[lnRep3Pos,6]') <> 'U'
    laOGFxFlt[lnRep3Pos,6] = &lcCrMemHdr..Rep3
  ENDIF
ENDIF

lnComm3Pos = ASCAN(laUsrField,"COMM3")
IF lnComm3Pos > 0
  lnComm3Pos = ASUBSCRIPT(laUsrField,lnComm3Pos,1)
  laUsrField[lnComm3Pos,6] = &lcCrMemHdr..Comm3
  IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
    laOGFxFlt[lnComm3Pos,6] = &lcCrMemHdr..Comm3
  ENDIF
ENDIF

lnComAm3Po = ASCAN(laUsrField,"COMMAMT3")
IF lnComAm3Po > 0
  lnComAm3Po = ASUBSCRIPT(laUsrField,lnComAm3Po,1)
  laUsrField[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
  IF TYPE('laOGFxFlt[lnComAm3Po,6]') <> 'U'
    laOGFxFlt[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
  ENDIF
ENDIF
        
SELECT (lcAlias)        
*-- End of lfGetRep3.

*!**************************************************************************
*! Name      : lfCrUpCom3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Update comm 3 for Return from Invoice.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Credit Memo screen.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('CRUPCOM3',10))
*!**************************************************************************
*
FUNCTION lfCrUpCom3
SELECT (lcCrMemHdr)
*-- Lock the current header record to grantee the phiscal update.
=RLOCK()
*-- Calculate the salesreps. charge back.
IF lnInvTrdDs <> 0
  *REPLACE CommAmt3 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..Comm3/100),2)) * -1
  REPLACE CommAmt3 WITH ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..Comm3/100),2)) * -1
ELSE
  *REPLACE CommAmt3 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..Comm3/100),2)) * -1
  REPLACE CommAmt3 WITH ABS(ROUND(laData[25] * (&lcCrMemHdr..Comm3/100),2)) * -1
ENDIF
UNLOCK

PRIVATE lnRep3Pos , lnComm3Pos , lnComAm3Po
lnRep3Pos = ASCAN(laUsrField,"REP3")
IF lnRep3Pos > 0
  lnRep3Pos = ASUBSCRIPT(laUsrField,lnRep3Pos,1)
  laUsrField[lnRep3Pos,6] = &lcCrMemHdr..Rep3
  IF TYPE('laOGFxFlt[lnRep3Pos,6]') <> 'U'
    laOGFxFlt[lnRep3Pos,6] = &lcCrMemHdr..Rep3
  ENDIF
ENDIF

lnComm3Pos = ASCAN(laUsrField,"COMM3")
IF lnComm3Pos > 0
  lnComm3Pos = ASUBSCRIPT(laUsrField,lnComm3Pos,1)
  laUsrField[lnComm3Pos,6] = &lcCrMemHdr..Comm3
  IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
    laOGFxFlt[lnComm3Pos,6] = &lcCrMemHdr..Comm3
  ENDIF
ENDIF

lnComAm3Po = ASCAN(laUsrField,"COMMAMT3")
IF lnComAm3Po > 0
  lnComAm3Po = ASUBSCRIPT(laUsrField,lnComAm3Po,1)
  laUsrField[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
  IF TYPE('laOGFxFlt[lnComm3Pos,6]') <> 'U'
    laOGFxFlt[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
  ENDIF
ENDIF

*-- End of lfCrUpCom3.

*!**************************************************************************
*! Name      : lfCalcAmt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Update comm amount 3.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice saving program (ARINV).
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('CALCAMT',10))
*!**************************************************************************
*
FUNCTION lfCalcAmt
m.COMMAMT3 = &lcHdrFile..Comm3*lnNetShipAmnt/100
IF SUBSTR(gcbasewind,4) = "ARDINV"
  PRIVATE lcAlias
  lcAlias = ALIAS()
  SELECT (lcHdrFile)
  =RLOCK()
  REPLACE CommAmt3 WITH m.CommAmt3
  UNLOCK
  
  PRIVATE lnComAm3Po
  lnComAm3Po = ASCAN(laUsrField,"COMMAMT3")
  IF lnComAm3Po > 0
    lnComAm3Po = ASUBSCRIPT(laUsrField,lnComAm3Po,1)
    laUsrField[lnComAm3Po,6] = &lcHdrFile..CommAmt3
    IF TYPE('laOGFxFlt[lnComAm3Po,6]') <> 'U'
      laOGFxFlt[lnComAm3Po,6] = &lcHdrFile..CommAmt3
    ENDIF
  ENDIF
  SELECT (lcAlias)  
ENDIF  
*-- End of lfCalcAmt.

*!**************************************************************************
*! Name      : lfConsRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Add 3rd Sales Rep from OrdHdr into Consolidated InvHdr.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice Sales Order.
*!**************************************************************************
*! Example   : =gfDoTriger('ARIINV',PADR('CONSREP3',10))
*!**************************************************************************
*
FUNCTION lfConsRep3
REPLACE Rep3  WITH m.Rep3  ,;
        Comm3 WITH 0
*-- End of lfConsRep3.

*!**************************************************************************
*! Name      : lfInvRep3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Add 3rd Sales Rep from OrdHdr into InvHdr.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice Sales Order.
*!**************************************************************************
*! Example   : =gfDoTriger('ARIINV',PADR('INVREP3',10))
*!**************************************************************************
*
FUNCTION lfInvRep3
REPLACE Rep3 WITH m.Rep3 , Comm3 WITH 0
*-- End of lfInvRep3.

*!**************************************************************************
*! Name      : lfUpdAmt3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Update comm amount 3.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Invoice sales order program (ARIINV).
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('UPDAMT',10))
*!**************************************************************************
*
FUNCTION lfUpdAmt3
m.COMMAMT3 = &lcInvHdr..ShipAmt*(1-&lcInvHdr..DiscPcnt/100)*(1-&lcInvHdr..Trde_Disc/100)
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcInvHdr)
=RLOCK()
REPLACE CommAmt3 WITH m.CommAmt3 * &lcInvHdr..Comm3/100
UNLOCK
=lfUserFlds() 
SELECT (lcAlias)
*-- End of lfUpdAmt3.

*!**************************************************************************
*! Name      : lfVoidCom3
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Void comm amount 3.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Direct Invoice screen , Return memo.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('VOIDCOM3',10))
*!**************************************************************************
*
FUNCTION lfVoidCom3
REPLACE VCOMMAMT3 WITH COMMAMT3 ;
        COMMAMT3  WITH 0
*-- End of lfVoidCom3.

*!**************************************************************************
*! Name      : lpUpdComAm
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : update commission amount.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Direct Invoice screen , Return memo.
*!**************************************************************************
*! Example   : =gfDoTriger('RMCRMEM',PADR('VOIDCOM3',10))
*!**************************************************************************
*
PROCEDURE lpUpdComAm
DO CASE
  CASE SUBSTR(gcbasewind,4) = "RMCRMEM"
    SELECT (lcCrMemHdr)
    *-- Lock the current header record to grantee the phiscal update.
    =RLOCK()
    *-- Calculate the salesreps. charge back.
    IF lnInvTrdDs <> 0
      REPLACE CommAmt3 WITH ABS(ROUND((laData[25] * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..Comm3/100),2)) * -1
    ELSE
      REPLACE CommAmt3 WITH ABS(ROUND(laData[25] * (&lcCrMemHdr..Comm3/100),2)) * -1
    ENDIF
    UNLOCK

    PRIVATE lnComAm3Po
    lnComAm3Po = ASCAN(laUsrField,"COMMAMT3")
    IF lnComAm3Po > 0
      lnComAm3Po = ASUBSCRIPT(laUsrField,lnComAm3Po,1)
      laUsrField[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
      IF TYPE('laOGFxFlt[lnComAm3Po,6]') <> 'U'
        laOGFxFlt[lnComAm3Po,6] = &lcCrMemHdr..CommAmt3
      ENDIF
    ENDIF
    CLEAR READ
  CASE SUBSTR(gcbasewind,4) = "ARDINV" OR SUBSTR(gcbasewind,4) = "ARIINV"
    =lfUpdAmt3()
    CLEAR READ
ENDCASE
*-- End of lpUpdComAm.

*!**************************************************************************
*! Name      : lfBuildOrd
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/01/2002
*! Purpose   : Build order for Direct Invoice.
*!**************************************************************************
*! Reference : C102676,1
*!**************************************************************************
*! Called From : Direct Invoice screen. 
*!**************************************************************************
*! Example   : =gfDoTriger('ARDINV',PADR('BUILDORD',10))
*!**************************************************************************
*
FUNCTION lfBuildOrd
REPLACE Rep3 WITH m.Rep3 , Comm3 WITH &lcHdrFile..Comm3
*-- End of lfBuildOrd.