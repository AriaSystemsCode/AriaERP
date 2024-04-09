*:***************************************************************************
*: Program file  : ARKRA20.PRG
*: Program desc. : Custom program to reverse cash entry or/and keyoff trans.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat Ltd.
*:***************************************************************************
*: C101631,1 KHM 10/25/1999
*:***************************************************************************

*-- To check the GL link.
llGlLink   = (UPPER(ALLTRIM(gfGetMemVar("M_LINK_GL"  ))) = "Y")

*-- To check if the SalesRep module is installed and the charge back for
*-- salesrep status.
llRepCBk   = ("SR" $ gcCmpModules) AND;
             (UPPER(ALLTRIM(gfGetMemVar("M_REPCB")))="Y")

*-- To check if the AP module is installed or not.
llApLink  = "AP" $ gcCmpModules

*-- Initializing the necessary variables.
lcGlFYear  = ""   && The fiscal year
lcGlPeriod = ""   && The fiscal period
STORE '' TO lcBrowseTl
lnMarker  = 0
lnMarker1 = 0
lcbTitle1 = 'Open cash payments'
lcbTitle2 = "Key off entries"
lcbTitle3 = "History"
llKey     = .F.
lcAcc     = SPACE(5)
lcAcc2    = SPACE(5)
lcFilter1 = "Account+Tran+DTOS(TranDate) = lcAcc AND !EMPTY(Account) AND Trantype ='4'"
llFound   = .F.
llBrowse  = .F.
llSel     = .T.
lnRec     = 0
llNewTag  = .F.
lnCount   = 1
lcGlSess  = ''
lcCheck   = SPACE(8)         && To search for the Check #
TMPGLDIS  = gfTempName()
TmpArhist = gfTempName()
TmpSelRec = gfTempName()
Tmp1      = gfTempName()

IF !gfSetup()
  RETURN
ENDIF

*-- To check if the date is valid or not.
IF !lfCheckPrd(gdSysDate,"lcGlFYear","lcGlPeriod")
  RETURN
ENDIF

*-- Opening and creating the necessary files.
=lfOpnFiles()

*-- Check if ARHIST file has the new tag 'ARHISTTS' to be used in 
*-- searching check#
DO WHILE !EMPTY(TAG(lnCount,"ARHIST")) .AND. !llNewTag
  llNewTag = (TAG(lnCount,"ARHIST") = 'ARHISTTS')
  lnCount = lnCount + 1
ENDDO

*--Places all current ON KEY LABELs on a stack in memory.
=lfClearKey()
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcBrowseTl)
PUSH KEY
ON KEY
lcSysMen = SET('SYSMENU')
SET SYSMENU ON
DEFINE PAD _BROWSE1 OF _MSYSMENU PROMPT "" KEY CTRL+B
DEFINE PAD _BROWSE2 OF _MSYSMENU PROMPT "" KEY CTRL+C
ON SELECTION PAD _BROWSE1 OF _MSYSMENU ACTIVATE WINDOW (lcbTitle1)
ON SELECTION PAD _BROWSE2 OF _MSYSMENU ACTIVATE WINDOW (lcbTitle3)

DO gcScrDir+'AR\ARKRA20.SPX'

SET RELATION OFF INTO ARHIST
RELEASE PAD _BROWSE1 OF _MSYSMENU
RELEASE PAD _BROWSE2 OF _MSYSMENU

SET SYSMENU &lcSysMen
*--Restores ON KEY LABELs that were placed on the stack
POP KEY

*!*************************************************************
*! Name      : lfBrowPay
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To browse the cash payments transactions.
*:*************************************************************
*! Example     : =lfBrowPay()
*!*************************************************************
Function lfBrowPay

SELECT CREDIT
GO TOP
lnMarker = RECNO()
BROWSE FIELDS ; 
       Marker = IIF(LnMarker=RECNO(),'',' ')  :H=' ':R:1:W=.F. ,;
       Select = IIF(!EOF(TMPSELREC),'»',' ') :H=' ':R:1:W=.F. ,;
       Tran     :H='Transaction No' :R ,;
       Trandate :H= 'Date' :R ,;  
       Desc     :H= 'Description' :R ,;
       Amount ,;
       Store    :H='Check #' :R ,;
       Batch    :H='Batch #' :R ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       FOR EVAL(LcFilter1);
       WHEN lfwBrow()   ;
       WINDOW ARKRA20B   ;
       IN WINDOW ARKRA20 ;
       TITLE (lcbTitle1)

*!*************************************************************
*! Name      : lfBrowHist
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To browse the key off transactions.
*:*************************************************************
*! Example     : =lfBrowHist()
*!*************************************************************
FUNCTION lfBrowHist

SELECT (TmpArhist)
GO TOP
lnMarker = RECNO()
BROWSE FIELDS ; 
       Marker = IIF(LnMarker=RECNO(),'',' ') :H=' ':R:1:W=.F. ,;
       Select = IIF(!EOF(TMPSELREC),'»',' ')  :H=' ':R:1:W=.F. ,;
       History  :R :H='History',;
       Histdate :R :H='Key-off Date' ,;
       Totdb    :R :H='Total Debits' ,;
       Totcr    :R :H='Total Credits' ,;
       Openamt  :R :H='Open Amount' ;    
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOEDIT           ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       WHEN lfwBrow()   ;
       WINDOW ARKRA20B   ;
       IN WINDOW ARKRA20 ;
       TITLE (lcbTitle3) 

*!*************************************************************
*! Name      : lfBrowline
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Browse key off entries for the key off being 
*!             pointed in the browse of key off sessions. 
*:*************************************************************
*! Example     : =lfBrowline()
*!*************************************************************
FUNCTION lfBrowline

SELECT ARHIST
BROWSE FIELDS ; 
       Marker = IIF(LnMarker1=RECNO(),'',' ')  :H=' ':R:1:W=.F. ,;
       Tran     :R :H='Transaction No',;
       Store    :R :H='Check #' ,;
       Trandate :R :H='Trans. Date' ,;
       Duedate  :R :H='Due Date' ,;
       Batch    :R :H='Batch No.' ,;
       Amount   :R ,;
       Desc     :R :H='Description' ;
       NOMENU           ;
       NOAPPEND         ;
       NODELETE         ;
       NOEDIT           ;
       NOWAIT           ;
       NOCLEAR          ;
       SAVE             ;
       KEY !EMPTY(ACCOUNT);
       WHEN lfwBroLine();
       WINDOW ARKRA20C   ;
       IN WINDOW ARKRA20 ;
       TITLE (lcbTitle2) 

*!*************************************************************
*! Name      : lfwBroLine
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : When function for the browse of key off entries.
*:*************************************************************
*! Example     : =lfwBroLine()
*!*************************************************************
FUNCTION lfwBroLine

lnMarker1 = RECNO("ARHIST")
SHOW WINDOW (lcbTitle2) SAME REFRESH

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : When function for browse of cash payments or key off. 
*:*************************************************************
*! Example     : =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

IF !llKey
   lnMarker = RECNO("CREDIT")
   SHOW WINDOW (lcbTitle1) SAME REFRESH
ELSE
  lnMarker  = RECNO(TMPARHIST)
  lnMarker1 = RECNO("ARHIST")
  SHOW WINDOW (lcbTitle2) SAME REFRESH
  SHOW WINDOW (lcbTitle3) SAME REFRESH
ENDIF
IF EOF(TMPSELREC)
  SHOW GET PbSelect,1 PROMPT '\<Select'
  llSel = .T.
ELSE
  SHOW GET PbSelect,1 PROMPT '\<UnSelect'
  llSel = .F.
ENDIF

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for the Account
*!*************************************************************
*! Calls     : lfTmpHist,lfBrowHist,lfBrowLine,lfShow,lfvPay,lfTmpHist.
*:*************************************************************
*! Example     : =lfvAcc()
*!*************************************************************
FUNCTION lfvAcc

IF lcAcc2 = lcAcc AND !EMPTY(lcAcc) AND !llBrowse
   RETURN
ENDIF  
lcAcc2    = lcAcc
llAccount = .F.
IF EMPTY(lcAcc) AND !llBrowse
  IF !llKey
    SHOW WINDOW (lcbTitle1) SAME REFRESH
  ELSE
    =lfTmpHist()
    =lfBrowHist()
    =lfBrowLine()
  ENDIF
  =lfShow()
  RETURN
ENDIF  
SELECT(TMPSELREC)
ZAP
IF !SEEK('M'+lcAcc,'CUSTOMER') OR llBrowse 
   IF !CusBrowM(@lcAcc , '' , 'M')
    lcAcc    = SPACE(5)
    llBrowse = .F.
    _CUROBJ  = OBJNUM(lcAcc)
    RETURN
  ELSE
    lcAcc = Account
  ENDIF
ENDIF  
SHOW GET LcAcc
IF !llKey
  WAIT 'Collecting cash payments for account# '+ LcAcc WINDOW NOWAIT
  =lfvPay()
  =SEEK(lcAcc,"CREDIT")
  LOCATE REST WHILE Account+Tran+DTOS(TranDate)= lcAcc;
              FOR TranType = '4'
  lnMarker = RECNO("CREDIT")
  SHOW WINDOW (lcbTitle1) SAME REFRESH
  IF !llFound
    *=gfDialog("I","No Payments have been found for account # " + ALLTRIM(lcAcc))
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "No Payments have been found for account # " + ALLTRIM(lcAcc)+".")
    llAccount = .T.
    _CUROBJ = OBJNUM(lcAcc)
  ENDIF
ELSE
  WAIT 'Collecting Key off sessions for account# '+ LcAcc WINDOW NOWAIT
  =lfTmpHist()
  lnMarker = RECNO(TMPARHIST)
  =lfBrowHist()
  =lfBrowLine()
  SHOW WINDOW (lcbTitle3) SAME REFRESH
  SHOW WINDOW (lcbTitle2) SAME REFRESH
  IF EOF(TmpArhist)
    *=gfDialog("I","No Key off sessions have been found for account # " + ALLTRIM(lcAcc))  
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "No Key off sessions have been found for account # " + ALLTRIM(lcAcc)+".")

    llAccount = .T.
    _CUROBJ   = OBJNUM(lcAcc)
  ENDIF
ENDIF
=lfShow()
llBrowse = .F.
WAIT CLEAR
IF llAccount = .T.
  RETURN
ENDIF  
IF !llKey
  KEYBOARD "{CTRL+B}"    
ELSE
  KEYBOARD "{CTRL+C}"    
ENDIF

*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To validate the close button.
*:*************************************************************
*! Example     : =lfvClose()
*!*************************************************************
FUNCTION lfvClose

CLEAR READ

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To select the cash payments or key off transactions.
*:*************************************************************
*! Example     : =lfvSelect()
*!*************************************************************
FUNCTION lfvSelect

IF llSel
  IF !llKey
    *-- Check if the transaction date of the selected record is greater than 
    *-- the system date then do not allow to select the record.
     IF Credit.TranDate > gdSysDate    
      *=gfDialog("I","The transaction date should not be greater than"+;
                    " the reverse date. Cannot select.")
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
      "The transaction date should not be greater than the reverse date."+;
      " Cannot select.")
      RETURN
    ENDIF
    SHOW GET PbSelAll DISABLE

    INSERT INTO (TMPSELREC) (Key) VALUES (CREDIT.Tran)
    SELECT CREDIT
    lnRec = RECNO()
    =SEEK(lcAcc)  
    SCAN WHILE Account+Tran+DTOS(TranDate) = lcAcc FOR TranType = '4'
      IF EOF(TMPSELREC)
        SHOW GET PbSelAll ENABLE
        EXIT
      ENDIF
    ENDSCAN
  ELSE
    *-- Check if the transaction date of the selected record is greater than 
    *-- the system date then do not allow to select the record.
    IF &TmpArhist..Histdate > gdSysDate
      *=gfDialog("I","The transaction date should not be greater than"+;
                    " the reverse date. Cannot select.")
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
      "The transaction date should not be greater than the reverse date."+;
      " Cannot select.")
      RETURN
    ENDIF
    SHOW GET PbSelAll DISABLE
    INSERT INTO (TMPSELREC) (Key) VALUES (&TMPARHIST..History)
    SELECT (TMPARHIST)
    lnRec = RECNO()
    GO TOP
    SCAN WHILE !EOF()
      IF EOF(TMPSELREC)
        SHOW GET PbSelAll ENABLE
        EXIT 
      ENDIF
    ENDSCAN
  ENDIF
  GO LnRec
  =lfwBrow()  
  SHOW GET PbSelNone ENABLE
  SHOW GET PbProceed ENABLE
ELSE
  SELECT (TMPSELREC)
  DELETE
  GO TOP
  SHOW GET PbSelAll ENABLE
  IF EOF()
    SHOW GET PbSelNone DISABLE
    SHOW GET PbProceed DISABLE
  ELSE  
    SHOW GET PbSelNone ENABLE
    SHOW GET PbProceed ENABLE
  ENDIF
  =lfwBrow()  
ENDIF

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To select all cash payments or key off transactions.
*:*************************************************************
*! Example     : =lfvSelAll()
*!*************************************************************
FUNCTION lfvSelAll
PRIVATE llFound

llFound = .F.

SELECT (TMPSELREC)
ZAP
IF !llkey
  SELECT CREDIT
  lnRec = RECNO()
  SEEK LcAcc
  SCAN WHILE Account+Tran+DTOS(TranDate) = lcAcc FOR TranType = '4'
    IF Credit.TranDate <= gdSysDate
      INSERT INTO (TMPSELREC) (Key) VALUES (CREDIT.Tran)
    ELSE
      llFound = .T.
    ENDIF
  ENDSCAN
  IF llFound
    *=gfDialog("I","One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
  ENDIF
  GO lnRec
  SHOW WINDOW (lcbTitle1) SAME REFRESH
ELSE
  SELECT (TMPARHIST)
  LnRec = RECNO()
  GO TOP
  SCAN WHILE !EOF()
    IF &TmpArhist..Histdate <= gdSysDate
      INSERT INTO (TMPSELREC) (Key) VALUES (&TMPARHIST..History)
    ELSE
      llFound = .T.
    ENDIF  
  ENDSCAN
  IF llFound
    *=gfDialog("I","One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
  ENDIF
  GO lnRec
  SHOW WINDOW (lcbTitle3) SAME REFRESH
ENDIF
=lfwBrow()  
IF !EOF(TMPSELREC) 
  SHOW GET PbSelNone ENABLE
  SHOW GET PbProceed ENABLE
  SHOW GET PbSelAll  DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvSelNone
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To unselect all cash payments or key off transactions.
*:*************************************************************
*! Example     : =lfvSelNone()
*!*************************************************************
FUNCTION lfvSelNone

SHOW GET PbSelAll ENABLE
SHOW GET PbSelNone DISABLE
SHOW GET PbProceed DISABLE
SELECT (TMPSELREC)
ZAP
IF !llkey
  SHOW WINDOW (lcbTitle1) SAME REFRESH
ELSE
  SHOW WINDOW (lcbTitle3) SAME REFRESH
ENDIF
=lfwBrow()  

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To invert the selected key offs or payments.
*:*************************************************************
*! Example     : =lfvInvert()
*!*************************************************************
FUNCTION lfvInvert
PRIVATE llFOUND

llFound = .F.

SELECT (TMPSELREC)
GO TOP 
IF EOF()
  SHOW GET PbSelAll DISABLE
ELSE
  SHOW GET PbSelAll ENABLE
ENDIF  
IF !llkey
  SELECT CREDIT
  lnRec = RECNO()
  =SEEK(lcAcc)  
  SCAN WHILE Account+Tran+DTOS(TranDate) = lcAcc FOR TranType = '4'
    IF EOF(TMPSELREC)
      IF Credit.TranDate <= gdSysDate
        INSERT INTO (TMP1) (Key) VALUES (CREDIT.Tran)
      ELSE
        llFound = .T.
      ENDIF  
    ENDIF
  ENDSCAN
  *-- If found a transaction that has a date greater than the system date 
  *-- then display the message.
  IF llFound
    *=gfDialog("I","One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
  ENDIF
  SET RELATION TO Tran INTO (TMP1) ADDITIVE
ELSE
  SELECT (TMPARHIST)
  lnRec = RECNO()
  GO TOP
  SCAN WHILE !EOF()
    IF EOF(TMPSELREC)
      *-- Checking if there is one of the selected record has a transaction 
      *-- date greater than the system date then assgin .T. to llfound
      IF &TmpArHist..HistDate <= gdSysDate
        INSERT INTO (TMP1) (Key) VALUES (&TMPARHIST..History)
      ELSE
        llFound = .T.
      ENDIF  
    ENDIF
  ENDSCAN
  *-- If found a transaction that has a date greater than the system date 
  *-- then display the message.
  IF llFound
    *=gfDialog("I","One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "One or more transactions that fall after the reverse date"+;
                  " will be ignored.")
  ENDIF
  SET RELATION TO HISTORY INTO (TMP1) ADDITIVE
ENDIF
SET RELATION OFF INTO (TMPSELREC)
GO lnRec
SELECT (TMP1)
GO TOP
IF EOF()
  SHOW GET PbSelNone DISABLE
  SHOW GET PbProceed DISABLE
ELSE  
  SHOW GET PbSelNone ENABLE
  SHOW GET PbProceed ENABLE
ENDIF
lcSwap    = TMPSELREC
TMPSELREC = TMP1
TMP1      = lcSwap
SELECT (TMP1)
ZAP
IF !llkey
  SHOW WINDOW (lcbTitle1) SAME REFRESH
ELSE
  SHOW WINDOW (lcbTitle3) SAME REFRESH
ENDIF
=lfwBrow()  

*!*************************************************************
*! Name      : lfvProceed
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for proceed button that 
*!             reverse selected key offs or payments.
*!*************************************************************
*! Calls     : lfAccRev,lfUpdSale,lfUpdateD,lfUpdateC,
*!             lfDelhc,lfwBrow.
*:*************************************************************
*! Example     : =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

llDisable = .T.
IF !llkey
  SELECT CREDIT
  SET RELATION OFF INTO (TMPSELREC)
  SELECT (TMPSELREC)
  SCAN
    IF SEEK(lcAcc+Key,"CREDIT")  
      *IF gfDialog("I"," Are you sure you want to reverse payment # " ;
      + CREDIT.Tran , "\<Ok;\<Cancel" ) = 2
      
      IF gfModalGen('TRM00000B00030',.F.,.F.,.F.,;
      " Are you sure you want to reverse payment # " + CREDIT.Tran) = 2
        LOOP
      ENDIF
      WAIT 'Reverse Payment # ' + CREDIT.Tran WINDOW NOWAIT
      IF llGlLink
        WAIT 'Update general ledger.'  WINDOW NOWAIT
        *** 1) TOTAL GROSS MERCHANDISE     <DEBIT>   "CASH RECEIPT"
        *** CATEGORY KEY FOR "Cash Receipts"..........=> '002'
        lcOldAls = SELECT(0)
        SELECT GLDIST
        *lcOldOrd = ORDER()
        *SET ORDER TO GLDISTNO
        *lcMGlAcct = IIF(SEEK(Credit.Tran),GLDIST.GlAccount,'')
        *SET ORDER TO &lcOldOrd
        SELECT(lcOldAls)
        
        DO GLDIST WITH Credit.Link_Code,'002',CREDIT.Amount,'CR',;
                       Credit.Batch,gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',Credit.cAdjAcct
                       
        *** 2) MERCHANDISE DISCOUNT        <CREDIT>  "ACCOUNT RECEIVABLE"
        *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
        DO GLDIST WITH Credit.Link_Code,'001',-(CREDIT.Amount),'CR',;
                       Credit.Batch,gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',Credit.cArGlAcc
        
      ENDIF
      IF llApLink
        SELECT ApPayMnt
        =SEEK('A'+IIF(Credit.lNonAr,'N','A')+Credit.Store)
        LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = ;
                          'A'+IIF(Credit.lNonAr,'N','A')+Credit.Store ;
                    FOR   dPayDate = Credit.TranDate .AND. cPayClNo=lcAcc ;
                   .AND.  nPayAmnt = Credit.Amount
        IF FOUND()
          DELETE
        ENDIF
      ENDIF
      SELECT CREDIT
      =SEEK("M" + lcAcc,"Customer")
      REPLACE CUSTOMER.OPENCR WITH CUSTOMER.OPENCR - CREDIT.AMOUNT,;
              CUSTOMER.NETBAL WITH CUSTOMER.NETBAL - CREDIT.AMOUNT    
      DELETE
    ENDIF
  ENDSCAN
  IF llGlLink
    SELECT (TMPGLDIS)
    REPLACE ALL Glsession WITH lcGlSess
    *--Update GLDIST File
    USE
    SELECT GLDIST
    APPEND FROM (gcWorkDir+TMPGLDIS)
    =gfOpenFile(gcWorkDir+TMPGLDIS,'','EX')
    ZAP
  ENDIF
  SELECT CREDIT
  SET RELATION TO Tran INTO (TMPSELREC) 
  SHOW GET PbSelAll DISABLE
  SHOW GET PbInvert DISABLE
  IF SEEK(lcAcc,"CREDIT")
    SCAN WHILE Account+Tran+DTOS(TranDate) = lcAcc FOR Trantype = "4"
      SHOW GET PbSelAll ENABLE
      SHOW GET PbInvert ENABLE
      llDisable =.F.
      EXIT
    ENDSCAN
  ENDIF
  SHOW WINDOW (lcbTitle1) SAME REFRESH
ELSE
  =SEEK("M"+ lcAcc ,"Customer")
  SELECT (TMPARHIST)
  SET RELATION TO
  SELECT (TMPSELREC)
  GO TOP
  SCAN 
    IF SEEK(lcAcc+Key,"ARHIST")  
      SELECT ARHIST
      *IF gfDialog("I"," Are you sure you want to reverse key off session # " ;
                  + History , "\<Ok;\<Cancel" ) = 2
      IF gfModalGen('TRM00000B00030',.F.,.F.,.F.,;
      " Are you sure you want to reverse key off session # " + History) = 2
        LOOP
      ENDIF
      WAIT 'Reverse Key off # ' + History WINDOW NOWAIT
      IF !lfAccRev()
        *=gfDialog("I"," There are C/B and/or Credit on account entries generated in key off # " + history + " have already been keyeed off , Therefore unable to reverse this key off." )
        =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
        " There are C/B and/or Credit on account entries generated in key "+;
        "off # " + history + " have already been keyeed off , "+;
        "Therefore unable to reverse this key off." )
        LOOP
      ENDIF
      SELECT ARHIST
      =SEEK(lcAcc+&TMPSELREC..Key)
      SCAN WHILE ACCOUNT +History +Trantype+Tran = lcAcc + &TMPSELREC..Key
        lcAdjAcc  = cAdjAcct
        lcArGlAcc = cArGlAcc
        IF Trantype ="7" OR (Trantype ="2" AND Deb_adj = "Y")       
          IF llGlLink
            IF Trantype ="7"
        
              DO GLDIST WITH ArHist.Link_Code,'009',ArHist.Amount,'CA',ArHist.Tran, ;
               gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',lcAdjAcc

              DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'CA', ;
                 ArHist.Tran,gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',lcArGlAcc
            ELSE
        
               DO GLDIST WITH ArHist.Link_Code,'010',ArHist.Amount,'DA',ArHist.Tran, ;
                 gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',lcAdjAcc

               DO GLDIST WITH ArHist.Link_Code,'001',-(ArHist.Amount),'DA', ;
                  ArHist.Tran,gdSysDate,lcGlFYear,lcGlPeriod,'&TMPGLDIS',lcArGlAcc
            ENDIF
            IF llApLink
              SELECT ApPayMnt
              =SEEK('AA')
              LOCATE REST WHILE cPayType+cPayMeth+cPayDocNo+cBnkCode+cChkAcct = 'AA' ;
               FOR ((CPAYDOCNO = &TMPSELREC..Key) .OR. EMPTY(CPAYDOCNO)) ;
                   .AND.  dPayDate = ArHist.TranDate .AND. cPayClNo=lcAcc ;
                   .AND.  ABS(nPayAmnt) = ABS(ArHist.Amount)
              IF FOUND()
                DELETE
              ENDIF
            ENDIF
          ENDIF
          SELECT ARHIST
          IF llRepCBk .AND. Trantype ="7"
            =lfUpdSale(TranDate,Tran)
          ENDIF
          LOOP
        ENDIF
        DO CASE
          CASE INLIST(Trantype,"1","2","3")
            =lfUpdateD()
          CASE INLIST(Trantype,"0","4","5","6")
            =lfUpdateC()
          OTHERWISE
            =lfDelhc()
        ENDCASE
      ENDSCAN
      =SEEK(lcAcc+&TMPSELREC..Key)
      DELETE REST WHILE Account+History+Trantype+Tran = lcAcc + &TMPSELREC..Key    
      SELECT (TMPARHIST)  
      IF SEEK(lcAcc+&TMPSELREC..Key)  
        DELETE
      ENDIF
    ENDIF
  ENDSCAN
  IF llGlLink
    SELECT (TMPGLDIS)
    REPLACE ALL Glsession WITH lcGlSess
    *--Update GLDIST File
    USE

    SELECT GLDIST
    APPEND FROM (gcWorkDir+TMPGLDIS)
    =gfOpenFile(gcWorkDir+TMPGLDIS,'','EX')
    ZAP
  ENDIF
  SELECT (TMPARHIST)  
  GO TOP
  IF EOF()
    SHOW GET PbSelAll DISABLE
    SHOW GET PbInvert DISABLE
    SHOW GET PbSearch DISABLE
  ELSE
    SHOW GET PbSelAll ENABLE
    SHOW GET PbInvert ENABLE
    llDisable =.F.
  ENDIF
  SET RELATION TO lcAcc + History INTO ARHIST ADDITIVE
  SET RELATION TO HISTORY INTO (TMPSELREC) ADDITIVE
  SHOW WINDOW (lcbTitle3) SAME REFRESH

  *-- Age the customer
  SELECT ArHist
  DO (gcAppHome+"ARAGEAR.PRG") WITH lcAcc  
  SELECT (TMPARHIST)
    
ENDIF
SELECT (TMPSELREC)
ZAP
SHOW GET PbSelNone DISABLE
SHOW GET PbProceed DISABLE
=lfwBrow()  
IF llDisable 
  SHOW GET PbSelect DISABLE
ELSE
  SHOW GET PbSelect ENABLE
ENDIF
WAIT CLEAR

*!*************************************************************
*! Name      : lfvRb
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for the radio buttons.
*!*************************************************************
*! Calls     : lfvPay,lfwBrow,lfTmpHist,lfBrowHist,lfBrowLine,
*!             lfShow.  
*:*************************************************************
*! Example     : =lfvRb()
*!*************************************************************
FUNCTION lfvRb

IF (RbSelect = 1 AND !llKey) OR (RbSelect = 2 AND llKey) 
  RETURN
ENDIF  
SELECT (TMPSELREC)
ZAP
llAccount = .F.
IF RbSelect = 1
  WAIT 'Collecting cash payments for account# '+ lcAcc WINDOW NOWAIT
  SELECT (TMPARHIST)
  SET RELATION OFF INTO (TMPSELREC)
  llKey =.F.
  =lfvPay()
  HIDE WINDOW (LcbTitle2)
  HIDE WINDOW (LcbTitle3)
  =SEEK(lcAcc,"CREDIT")
  LOCATE REST WHILE Account+Tran+DTOS(Trandate) = lcAcc FOR TranType = '4'
  LnMarker = RECNO("CREDIT")
  SHOW WINDOW (lcbTitle1) SAME REFRESH
  =lfwBrow()  
  IF !llFound
    *=gfDialog("I","No Payments have been found for account # " + ALLTRIM(lcAcc))
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "No Payments have been found for account # " + ALLTRIM(lcAcc))
    llAccount = .T.
    _CUROBJ = OBJNUM(lcAcc)
  ENDIF
ELSE
  WAIT 'Collecting Key off sessions for account# '+ lcAcc WINDOW NOWAIT
  SELECT CREDIT
  SET RELATION OFF INTO (TMPSELREC)
  llKey = .T.
  =lfTmpHist()
  HIDE WINDOW (LcbTitle1)

  *ZOOM WINDOW ARKRA20B NORM FROM 5.7, 2.4 SIZE 6.5, WCOLS("ARKRA20C")
  ZOOM WINDOW ARKRA20B NORM FROM 7.5, 1.4 SIZE 6.5, WCOLS("ARKRA20C")

  lnMarker = RECNO(TMPARHIST)
  =lfBrowHist()
  =lfBrowLine()
  =lfwBrow()  
  IF EOF(TmpArhist)
    *=gfDialog("I","No Key off sessions have been found for account # " + ALLTRIM(lcAcc))
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
    "No Key off sessions have been found for account # " + ALLTRIM(lcAcc))
    llAccount = .T.
    _CUROBJ = OBJNUM(lcAcc)
  ENDIF    
ENDIF  
=lfShow()
WAIT CLEAR
IF llAccount = .T.
  RETURN
ENDIF
IF !llKey 
  KEYBOARD "{CTRL+B}" 
ELSE
  KEYBOARD "{CTRL+C}" 
ENDIF

*!*************************************************************
*! Name      : lfTmpHist
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function to create temporary file contain key off sessions.
*:*************************************************************
*! Example     : =lfTmpHist()
*!*************************************************************
FUNCTION lfTmpHist

*--Create temporary file for key off 
SELECT DISTINCT Account,History , Histdate , Totdb , Totcr ,Openamt ;
FROM ARHIST ;
WHERE ACCOUNT+HISTORY+TRANTYPE+TRAN = LcAcc ;
AND (!EMPTY(Totdb) OR !EMPTY(Totcr));
INTO TABLE (gcWorkDir+TmpArhist)
IF EMPTY(TAG(1))
  INDEX ON Account+History+DTOS(Histdate) TAG (TMPARHIST)
ENDIF
GO TOP
SET RELATION TO lcAcc + History INTO ARHIST ADDITIVE

*!*************************************************************
*! Name      : lfvPay
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : function to check if there are open payments for 
*!             entered account or not.
*:*************************************************************
*! Example     : =lfvPay()
*!*************************************************************
FUNCTION lfvPay
llFound = .F.
SELECT CREDIT
=SEEK(lcAcc)  
SCAN WHILE Account+Tran+DTOS(TranDate)= lcAcc FOR TranType = '4'
  IF llFound
    EXIT
  ENDIF
  llFound = .T.
ENDSCAN

*!*************************************************************
*! Name      : lfShow
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function to control disabling or enabling for controls.
*:*************************************************************
*! Example     : =lfShow()
*!*************************************************************
FUNCTION lfShow

IF EMPTY(lcAcc)
  SHOW GETS DISABLE
  SHOW GET LcAcc ENABLE
  SHOW GET IbBrowCust ENABLE
  SHOW GET PbClose ENABLE
  RETURN
ENDIF
IF !llkey
  IF !llFound 
    SHOW GETS DISABLE
    SHOW GET RbSelect ENABLE
    SHOW GET LcAcc ENABLE
    SHOW GET IbBrowCust ENABLE
    SHOW GET PbClose ENABLE
  ELSE
    SELECT CREDIT
    SET RELATION TO Tran INTO (TMPSELREC) 
    SHOW GETS ENABLE
    SHOW GET PbSelNone DISABLE
    SHOW GET PbProceed DISABLE
    SHOW GET PbSearch DISABLE
  ENDIF  
ELSE
  IF EOF(TmpArhist) 
    SHOW GETS DISABLE
    SHOW GET RbSelect ENABLE
    SHOW GET LcAcc ENABLE
    SHOW GET IbBrowCust ENABLE
    SHOW GET PbClose ENABLE
  ELSE
    SELECT (TMPARHIST)
    IF EMPTY(TARGET(2))
      SET RELATION TO HISTORY INTO (TMPSELREC) ADDITIVE
    ENDIF
    SHOW GETS ENABLE
    SHOW GET PbSelNone DISABLE
    SHOW GET PbProceed DISABLE
    SHOW GET PbSearch ENABLE
  ENDIF  
ENDIF
SHOW GET IBKRA200A ENABLE
SHOW GET IBKRA200B ENABLE

*!*************************************************************
*! Name      : lfvSearch
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for search button.
*:*************************************************************
*! Example     : =lfvSearch()
*!*************************************************************
FUNCTION lfvSearch

lcCheck = SPACE(8)
PUSH KEY
ON KEY
DO gcScrDir+'AR\ARKRA20E.SPX'
POP KEY
KEYBOARD "{CTRL+C}"    

*!*************************************************************
*! Name      : lfvFind
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function to search for check # of payments
*:*************************************************************
*! Example     : =lfvFind()
*!*************************************************************
 FUNCTION lfvFind

llFind =.F.
SELECT (TMPARHIST)
lcCurrKey = lcAcc+History
SET RELATION OFF INTO ARHIST
IF !llNewTag
  SELECT ARHIST
  =SEEK(lcAcc)
  SCAN REST WHILE Account = lcAcc FOR Trantype ="4"
    IF STORE = lcCheck
      lcCurrKey = lcAcc+ History
      llFind = .T.
      EXIT
    ENDIF
  ENDSCAN
ELSE
  SET ORDER TO TAG ARHISTTS IN ARHIST
  IF SEEK(lcAcc+"4"+lcCheck,"ARHIST")
    lcCurrKey = lcAcc+ ARHIST.History
    llFind = .T.
  ENDIF
ENDIF
SET ORDER TO TAG ARHISTHT IN ARHIST
SELECT (TMPARHIST)
SET RELATION TO lcAcc + History INTO ARHIST ADDITIVE
=SEEK(lcCurrKey)
IF !llFind
  *=gfDialog("I","No Key off session has payments with check # " + ALLTRIM(LcCheck))
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
  "No Key off session has payments with check # " + ALLTRIM(LcCheck))
  _CUROBJ = OBJNUM(LcCheck)
ELSE
  CLEAR READ
ENDIF
SHOW WINDOW (lcbTitle3) SAME REFRESH
=lfwBrow()  

*!*************************************************************
*! Name      : lfvCheck
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for Check #.
*:*************************************************************
*! Example     : =lfvCheck()
*!*************************************************************
FUNCTION lfvCheck

IF EMPTY(LcCheck)
  SHOW GET PbFind DISABLE
  RETURN
ENDIF
SHOW GET PbFind ENABLE

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : The validation function for cancel button.
*:*************************************************************
*! Example     : =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel
CLEA READ

*!*************************************************************
*! Name      : lfAccRev
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : function to determine if there are charge back or
*!             credit on account already keyeed off found in the 
*!             key off selected to revese.
*:*************************************************************
*! Example     : =lfAccRev()
*!*************************************************************
FUNCTION lfAccRev

SCAN WHILE ACCOUNT +History +Trantype+Tran = lcAcc + &TMPSELREC..Key 
  IF (Trantype = "8" AND !SEEK(LcAcc +Tran,"DEBIT")) OR;
     (Trantype = "9" AND !SEEK(LcAcc +Tran,"CREDIT")) 
    RETURN .F.
  ENDIF   
ENDSCAN

*!*************************************************************
*! Name      : lfUpdateD
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function that udpate debit file.
*:*************************************************************
*! Example     : =lfUpdateD()
*!*************************************************************
FUNCTION lfUpdateD

IF !SEEK(ArHist.Account+ArHist.Tran,'Debit')
  INSERT INTO DEBIT;
   (Account,Store,Trantype,Trancode,Tran,Trandate,Chgbk_date,Desc,Reference,;
    Amount,Batch,Duedate,cFacCode,Dsc_amt,dadd_date,cadd_time,;
    cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
    dPostDate) ;
   VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
           ARHIST.Tran,ARHIST.Trandate,ARHIST.Chgbk_date,ARHIST.Desc,;
           ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.Duedate,;
           ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
           ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
           ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate)
ELSE
  REPLACE Debit.Amount WITH Debit.Amount + ARHIST.Amount
ENDIF
*!*************************************************************
*! Name      : lfUpdateC
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function that update credit file.
*:*************************************************************
*! Example     : =lfUpdateC()
*!*************************************************************
FUNCTION lfUpdateC

INSERT INTO CREDIT;
 (Account,Store,Trantype,cCreditCod,Tran,Trandate,Credt_date,Desc,Reference,;
  Amount,Batch,History,cFacCode,Dsc_amt,dadd_date,cadd_time,;
  cadd_user,Glflag,Mon_flg,Link_code,cCurrCode,nCurrUnit,nExRate,;
  dPostDate) ;
 VALUES (ARHIST.Account,ARHIST.Store,ARHIST.TranType,ARHIST.Trancode,;
         ARHIST.Tran,ARHIST.Trandate,ARHIST.Credt_date,ARHIST.Desc,;
         ARHIST.Reference,ARHIST.Amount,ARHIST.Batch,ARHIST.History,;
         ARHIST.cFacCode,ARHIST.Dsc_amt,gdSysDate,TIME(),gcuser_id,;
         ARHIST.Glflag,ARHIST.Mon_flg,ARHIST.Link_code,ArHist.cCurrCode,;
         ArHist.nCurrUnit,ArHist.nExRate,ArHist.dPostDate)

*!*************************************************************
*! Name      : lfDelhc
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function that delete credit on account or charge back.
*!             when reversing the key off.
*:*************************************************************
*! Example     : =lfDelhc()
*!*************************************************************
FUNCTION lfDelhc
IF Trantype = "8"
  IF SEEK(lcAcc+Tran,"DEBIT")
    SELECT DEBIT
    DELETE
  ENDIF
ELSE
  IF SEEK(lcAcc+Tran,"CREDIT")
    SELECT CREDIT
    DELETE
  ENDIF
ENDIF
SELECT ARHIST

*!*************************************************************
*! Name      : lfUpdSale
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Updating SalesRep file
*:*************************************************************
*! Example     : =lfUpdSale()
*!*************************************************************
FUNCTION lfUpdSale
PARAMETERS ldDate,lcTrano
PRIVATE lnCount,lnAlias

lnAlias = SELECT()
lnCount = 0  &&& number of reps
=SEEK('M'+lcAcc,"CUSTOMER")
lcRep1 = Customer.Salesrep
lcRep2 = Customer.Rep2

IF SEEK(lcRep1+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  lnCount = lnCount + 1
  IF SEEK(lcRep1,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    REPLACE Current WITH Current - RepComm.Amount;
            Balance WITH Balance - RepComm.Amount
    UNLOCK
    SELECT REPCOMM
    DELETE
  ENDIF
ENDIF
IF SEEK(lcRep2+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
  lnCount = lnCount + 1
  IF SEEK(lcRep2,'SalesRep')
    SELECT SalesRep
    =RLOCK()
    REPLACE Current WITH Current - RepComm.Amount;
            Balance WITH Balance - RepComm.Amount
    UNLOCK
    SELECT REPCOMM
    DELETE
  ENDIF
ENDIF  
SELECT SalesRep
GO TOP
SCAN WHILE lnCount < 2 AND !EOF()
  * check for the status of the commission is payed or not
  IF SEEK(SalesRep.Repcode+DTOS(ldDate)+lcTrano,"RepComm") AND (RepComm.Status ="O")
    SELECT SalesRep
    =RLOCK()
    REPLACE Current WITH Current - RepComm.Amount;
            Balance WITH Balance - RepComm.Amount
    UNLOCK
    SELECT REPCOMM
    DELETE
    SELECT SalesRep
    lnCount = lnCount + 1 
  ENDIF
ENDSCAN
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfDKRA200
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : READ Deactivate function of screen INV700F
*:*************************************************************
*! Example     : =lfDKRA200()
*!*************************************************************
FUNCTION lfDKRA200
IF WONTOP() = lcbTitle1 .OR. WONTOP() = lcbTitle2 .OR. WONTOP() = lcbTitle3
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC  DO lpTab WITH 'ARKRA20D', 'pbClose'
  IF WONTOP() = lcbTitle1
    ON KEY LABEL TAB     DO lpTab WITH 'ARKRA20D','pbSelect'
    ON KEY LABEL BACKTAB DO lpTab WITH 'ARKRA20A','RbSelect'
  ENDIF  
  IF WONTOP() = lcbTitle2
    ON KEY LABEL TAB     DO lpTab WITH 'ARKRA20D','pbSelect'
    ON KEY LABEL BACKTAB DO lpTab WITH 'ARKRA20A','ibKra200a'
  ENDIF  
  IF WONTOP() = lcbTitle3
    ON KEY LABEL TAB     DO lpTab WITH 'ARKRA20A','ibKra200b'
    ON KEY LABEL BACKTAB DO lpTab WITH 'ARKRA20A','PbSearch'
  ENDIF  
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Trap of tab key.
*:*************************************************************
*! Example     : =lpTab()
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Read activate function.
*:*************************************************************
*! Example     : =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

=lfClearKey()
ON KEY LABEL ALT+R ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfvActBrow 
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : Function to activate browse function.
*:*************************************************************
*! Example     : =lfvActBrow ()
*!*************************************************************
FUNCTION lfvActBrow 
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ  = OBJNUM(&lcObjName)
KEYBOARD "{ENTER}"


*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To open and create the ncessary files.
*:*************************************************************
*! Example     : =lfOpnFiles()
*!*************************************************************
FUNCTION lfOpnFiles

IF llGlLink
  =gfOpenFile(gcDataDir+'GLDIST','GLDISTNO','SH')
  SET ORDER TO 
  =AFIELDS(laFileStru)
  CREATE TABLE (gcWorkDir+TMPGLDIS) FROM ARRAY laFileStru
  lcGlSess = gfSequence('GLSESSION')
ENDIF
IF llApLink
  =gfOpenFile(gcDataDir+'ApPayMnt','TypMethDoc','SH')
ENDIF  

= gfOpenFile(gcDataDir+'DEBIT','DEBIT','SH')
= gfOpenFile(gcDataDir+'CREDIT','CREDIT','SH')
= gfOpenFile(gcDataDir+'ARHIST','ARHISTHT','SH')
= gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')

IF llRepCBk
  = gfOpenFile(gcDataDir+'REPCOMM','REPCOMM','SH')
  = gfOpenFile(gcDataDir+'SalesRep','SalesRep','SH')
ENDIF
CREATE CURSOR (TMPSELREC) (Key C(6))
INDEX ON KEY TAG KEY

CREATE CURSOR (TMP1) (Key C(6))
INDEX ON KEY TAG KEY


*!*************************************************************
*! Name      : lfCheckprd
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To validate the transaction date.
*:*************************************************************
*! Example     : =lfCheckprd()
*!*************************************************************
FUNCTION lfCheckprd
PARAMETERS ldDate,lcFYear,lcPeriod

PRIVATE lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',gcAct_Comp)
lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  USE lcSysDir+'SYCCOMP' ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP') 
  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
                  'System date ' + lcDate + ' belongs to prior period.')
    ELSE    && Period not closed. Check if it is a future period
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD
        =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
                    'System date ' + lcDate + ' belongs to a future period.')
      ENDIF
    ENDIF    
  ENDIF  
  USE IN SBTCOMP
ELSE
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
  =SEEK(gcPrnt_Cmp,'SYCCOMP')
  IF 'GL' $ SYCCOMP.mModlset
    USE (gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    *-- Variable that hold the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    llAllPBB = TGLSETUP.LSETALBBE
    *-- Variable that showes the Allow posting before beginning balance (End)
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    *-- Variable that showes the Allow posting before beginning balance (Start)
    *-- AAMER 11/12/98
    *-- .T. is assigend as default because we need not to check
    *-- if the GL module not installed or not linked
    llAllPBB = .T.
    *-- Variable that hold the Allow posting before beginning balance (End)
  ENDIF  
  =gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  =gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
  llContinue = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)                 
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period     
  ENDIF  
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(SYCCOMP.CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date

    *-- Check if the system is linked to GL And Allow posting before beginning Balance (Start)
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
    *-- Check if the system is linked to GL And Allow posting before beginning Balance (End) 
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
                  'System date ' + lcDate + ' belongs to prior period.')                
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD
        =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
                    'System date ' + lcDate + ' belongs to a future period.')
      ENDIF
    ENDIF    
  ENDIF  
ENDIF
IF !llContinue             && There is an error.
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,;
  'System date ' + lcDate + ' ' + lcErrorM1 +;
  ' Cannot proceed with Reverse Cash Entry/Key off. Please check the system date.')
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/25/1999
*! Purpose   : To clear the keys
*:*************************************************************
*! Example     : =lfClearKey()
*!*************************************************************
FUNCTION lfClearKey

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 
