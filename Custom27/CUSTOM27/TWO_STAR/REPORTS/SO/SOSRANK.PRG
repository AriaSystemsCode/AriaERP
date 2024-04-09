*:***************************************************************************
*: Program file  : SOSRANK.PRG
*: Program desc. : Sales Ranking Report for TWO10
*: For Report    : (SOSRANK.FRX)
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry Mohamed (MHM)
*: Date          : 01/22/2004
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,lfSeTSRep,lfGetMem,lfRankFile
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOSRANK
*:***************************************************************************
*: This Report Program is due to C037308 ...
*:***************************************************************************
*: Modifications :
*:B037936,1 NNA 05/04/2004 Fix bug that the amounts of the sales or the booking not correct
*:B037936,1 NNA            And Make some Changes in the reprot (Prg. and Frx) like :
*:B037936,1 NNA            *-- Add Order Status -Sort By - Rank by Both - Optional Title) to The OP.
*:B037936,1 NNA            *-- Add New Column to the [FRX] To Show The BookAmt if the user Select To Rank By both
*:B037936,1 NNA            *-- Add subTotal To the [FRX] on the SalesRep. If sorting by Rep.
*:B037936,1 NNA            *-- Add Optional Title Field To the [FRX]
*:B037936,1 NNA            *-- Add field (Have Space) to The Page Footer To make A separate Between pages
*:B126530,1 NNA 04/10/2005 Make some changes in the Report as Follow :
*:B126530,1 NNA            *-- Add New Column to the [FRX] To Show The OpenAmt if the user Select To Rank By both
*:B126530,1 NNA            *-- Get net of the Booked Amount that by substract the Cancelled Amount from the Booked
*:B126530,1 NNA            *-- Get the Booked and the Opened Amount within the Ordline.complete date Range not the ordhdr
*:***************************************************************************
lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

lcStTime = TIME()
STORE '' To m.Account , m.Name , m.buyer , m.phone , m.Rep 
*B037936,1 NNA (Begin) Define two new Variables
*-- AMTBOOk   : To Hold the Booked Amount From the OrdHdr if User select to Rank By Both
*-- lnAmtBoth : To loop twice (once for Collect Sales amount from the InvHdr and the other to collect
*--             Booked Amount From the OrdHdr that only if User select to Rank By Both
*STORE 0 TO m. Amount

*B126530,1 NNA 04/10/2005 (Begin) Add new Variable to hold the Open Amount
*STORE 0  TO m.Amount , m.AMTBOOK
STORE 0  TO m.Amount , m.Amtbook , m.Amtopen
*B126530,1 NNA (End)

STORE 1  TO lnAmtBoth
*B037936,1 NNA (End)
*--change all table according to Sales Or Book
IF lcRpRankBy = "S" 
  lcRpExp = STRTRAN(lcRpExp, 'ORDHDR', 'INVHDR')

*B037936,1 NNA (Begin) change the Date filter in ordhdr file from 'ENTERED' To 'COMPLETE' Date
  *lcRpExp = STRTRAN(lcRpExp, 'ENTERED', 'INVDATE')
  lcRpExp = STRTRAN(lcRpExp, 'COMPLETE', 'INVDATE')  
*B037936,1 NNA (End)
  
ELSE
  lcRpExp = STRTRAN(lcRpExp, 'INVHDR', 'ORDHDR')
*B037936,1 NNA (Begin) change the Date filter in ordhdr file from 'ENTERED' To 'COMPLETE' Date
  *lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'ENTERED')
  lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'COMPLETE')
*B037936,1 NNA (End)  
ENDIF

IF USED(RANK_FILE)
  USE IN (RANK_FILE)
ENDIF
= lfRankFile()

*B037936,1 NNA (Begin) If the user Did not select to Rank By Both (sales and Book)
IF lcRpRankBy<>'T'
*B037936,1 NNA (End)

  IF lcRpRankBy = "S"
    SELECT INVHDR
    SET RELATION TO 'M' + Account INTO CUSTOMER
    SET FILTER TO Status<> "V"
  ELSE
    SELECT ORDHDR
    *B126530,1 NNA 04/10/2005 (Begin) Ini
    SET RELATION TO OrdHdr.cordtype+ OrdHdr.order INTO OrdLine ADDITIVE
    *B126530,1 NNA (End)
    
    SET RELATION TO "M"+Account INTO CUSTOMER

    *B037936,1 NNA (Begin) filter To any order status as user chose
    *SET FILTER TO Status<> "X"
    lcRpExp = lcRpExp + IIF(lcRpOrdSt='A','',[ AND (OrdHdr.Status $"] + lcRpOrdSt + [")] )              
    *B126530,1 NNA 04/10/2005 (BEGIN) If user didn't select to see the cancelled orders then I filter
    *B126530,1 NNA            to orders that didn't cancelled
    IF !("X" $ lcRpOrdSt)
      SET FILTER TO Status<> "X"
    ENDIF
    *B126530,1 NNA (End)
    *B037936,1 NNA (End)    

  ENDIF

*B037936,1 NNA (Begin) If the user selected to Rank By Both (sales and Book), I Loop Twice,Once
*B037936,1 NNA         For the Sales Amount (InvHdr) and the other for The Booked Amount (OrdHdr)
*B037936,1 NNA         and I stoped Scan Command Here To use It As A function To Be easy to Dealing With
ELSE
  DO WHILE lnAmtBoth <> 3              && loop Twice
    IF lnAmtBoth = 1
      lcRpExp = STRTRAN(lcRpExp, 'ORDHDR', 'INVHDR')
     
      *B037936,1 NNA (Begin) change the Date filter in ordhdr file from 'ENTERED' To 'COMPLETE' Date
      *lcRpExp = STRTRAN(lcRpExp, 'ENTERED', 'INVDATE')
      lcRpExp = STRTRAN(lcRpExp, 'COMPLETE', 'INVDATE')
      *B037936,1 NNA (End)
      
      SELECT INVHDR
      SET RELATION TO 'M' + Account INTO CUSTOMER
      SET FILTER TO Status<> "V"
      = lfScanRec()
      lnAmtBoth = lnAmtBoth + 1
    ENDIF
    IF lnAmtBoth = 2    
      lcRpExp = STRTRAN(lcRpExp, 'INVHDR', 'ORDHDR')
      *B037936,1 NNA (Begin) change the Date filter in ordhdr file from 'ENTERED' To 'COMPLETE' Date
      *lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'ENTERED')
      lcRpExp = STRTRAN(lcRpExp, 'INVDATE', 'COMPLETE')      
      *B037936,1 NNA (End)       
      SELECT ORDHDR
      SET RELATION TO "M"+Account INTO CUSTOMER
      
      *B126530,1 NNA 04/10/2005 (Begin) Make Relation Between OrdHdr and Ordline
      SET RELATION TO OrdHdr.cordtype+ OrdHdr.order INTO OrdLine ADDITIVE
      *B126530,1 NNA (End)
      
      lcRpExp = lcRpExp + IIF(lcRpOrdSt='A','',[ AND (OrdHdr.Status $"] + lcRpOrdSt + [")] )              
      *B126530,1 NNA 04/10/2005 (BEGIN) If user didn't select to see the cancelled orders then I filter
      *B126530,1 NNA            to orders that didn't cancelled
      IF !("X" $ lcRpOrdSt)
        SET FILTER TO Status<> "X"
      ENDIF
      *B126530,1 NNA (End)
      = lfScanRec()
      lnAmtBoth = lnAmtBoth + 1      
    ENDIF
  ENDDO
ENDIF
*-- Scan all invoices in selected critria.
*SCAN FOR &lcRpExp
*  =lfGetMem()  && Fill Memory variables with its corresponding values.
*-- if you find this style in temp. file
*  IF SEEK(m.Rep+m.account,RANK_FILE)
*    SELECT (RANK_FILE)
*    REPLACE Amount    WITH Amount    + m.Amount 
*  ELSE  && Add new style record.
*    IF !EMPTY(m.account) OR !EMPTY(m.Rep)
*      INSERT INTO (RANK_FILE) FROM MEMVAR
*    ENDIF
*  ENDIF
*ENDSCAN
IF lcRpRankBy<>'T'           && if the user didn't select to Rank by Both
  = lfScanRec()    
ENDIF
*B037936,1 NNA (End)

SELECT (RANK_FILE)
IF RECCOUNT(RANK_FILE) > 0
  IF lcRpSortBy='M' 
    INDEX ON STR(Amount,14)+Rep+Account TAG (RANK_FILE1) OF (RANK_FILE) DESC
  ENDIF
  IF lcRpSortBy='R' 
    INDEX ON Rep+STR(Amount,14) TAG (RANK_FILE1) OF (RANK_FILE) DESC
   ENDIF
  IF lcRpSortBy='A' 
    INDEX ON Account TAG (RANK_FILE1) OF (RANK_FILE)
  ENDIF
ENDIF  

*-- Calculate spent time in collecting data.
DO gfDispRe WITH EVAL('lcRpForm')
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------

*!*************************************************************
*! Name      : lfRankFile
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Create temporary cursor.
*!*************************************************************
*! Called from : Program code, OG when function.
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRankFile()
*!*************************************************************
*
FUNCTION lfRankFile

*B037936,1 NNA (Begin) Add New Field For Amount To use It if the User Selected To Rank By Both (Sales And Booke)
*CREATE CURSOR (RANK_FILE) (Account C(6),Name C(20), Buyer C(15),;
                           Phone C(15), Rep C(3), Amount N(14,2))                           
*B126530,1 NNA 04/10/2005 (Begin) Add new Field to the Temp file to Store the Open amount
*CREATE CURSOR (RANK_FILE) (Account C(6),Name C(20), Buyer C(15),;
                           Phone C(15), Rep C(3), Amount N(14,2), AMTBOOK N(14,2))                           
CREATE CURSOR (RANK_FILE) (Account C(6),Name C(20), Buyer C(15),;
                           Phone C(15), Rep C(3), Amount N(14,2), AMTOPEN N(14,2), AMTBOOK N(14,2))                           
*B126530,1 NNA (End)
*B037936,1 NNA (End)

INDEX ON Rep+Account TAG (RANK_FILE) OF (RANK_FILE)

*-- end of lfRankFile.
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Go top in Sales Rep file.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSerSRep()
*!**************************************************************************
FUNCTION lfSetSRep
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SalesRep
   SET ORDER TO TAG SalesRep
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SalesRep 
    SET ORDER TO 
ENDCASE
*-- End of lfSetSRep.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
SELECT Customer
SET ORDER TO Customer
GO TOP

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : NADER NABIL (NNA)
*! Date      : 05/03/2004
*! Purpose   : Refresh the OPtion Grid if the [Rank by] Changed
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfClrRead()
*!*************************************************************
*!B03793,1
FUNCTION lfClrRead
CLEAR READ

*--END OF lfClrRead
*!*************************************************************
*! Name      : lfScanRec
*! Developer : Nader Nabil (NNA)
*! Date      : 05/05/2004
*! Purpose   : Scan Files For Report Expression
*!*************************************************************
*! Called from : Program code
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfScanRec()
*!*************************************************************
*!B037936,1
FUNCTION lfScanRec
*-- Scan all invoices in selected critria.
SCAN FOR &lcRpExp
  =lfGetMem()  && Fill Memory variables with its corresponding values.

  *-- if you find this style in temp. file
  IF SEEK(m.Rep+m.account,RANK_FILE)

    SELECT (RANK_FILE)
    IF lnAmtBoth = 1
      REPLACE Amount    WITH Amount    + m.Amount 
      STORE 0 to m.Amount
    ENDIF
    IF lnAmtBoth = 2    

      *B126530,1 NNA 04/10/2005 (Begin) Add the Open Amount to the Temp file
      *REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK
      *STORE 0 TO m.AMTBOOK
      REPLACE AMTBOOK   WITH AMTBOOK   + m.AMTBOOK , AMTOPEN WITH AMTOPEN + m.AMTOPEN
      STORE 0 TO m.AMTBOOK,AMTOPEN
      *B126530,1 NNA (End)
    ENDIF
  ELSE  && Add new style record.
    IF !EMPTY(m.account) OR !EMPTY(m.Rep)
      *B126530,1 NNA 04/10/2005 (Begin) Store 0 to m.Amtopen
      
      *INSERT INTO (RANK_FILE) FROM MEMVAR
      *STORE 0 to m.Amount 
      *STORE 0 TO m.AMTBOOK           
      IF m.Amount<>0 OR m.AMTBOOK<>0 OR m.AMTOPEN<>0
        INSERT INTO (RANK_FILE) FROM MEMVAR
      ENDIF
      STORE 0 to m.Amount , m.AMTBOOK , m.AMTOPEN
      *B126530,1 NNA (End)
    ENDIF
  ENDIF
ENDSCAN

*-- end of lfScanRec.
*!*************************************************************
*! Name      : lfGetMem
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/22/2004
*! Purpose   : Fill RANK_FILE memory variables with its corresponding data.
*!*************************************************************
*! Called from : Program code,
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetMem()
*!*************************************************************
FUNCTION lfGetMem

m.Account   = Customer.Account
m.Name      = Customer.btName
m.buyer     = Customer.buyer
m.phone     = Customer.phone1
*B037936,1 NNA 05/04/2004 (Begin) stop getting sales rep from the customer file because there are 
*B037936,1 NNA            many Reps in the InvHdr or the OrdHdr does't match the customer Rep.
*m.Rep       = Customer.salesrep
IF lcRpRankBy <> "T"
*B037936,1 NNA (End)

  IF lcRpRankBy = "S"
    *B037936,1 NNA 05/04/2004 (Begin) if the Rank by Sales , get the Rep from the InvHdr
    m.Rep   =InvHdr.Rep1 
    *B037936,1 NNA (End)
    *B126530,1 NNA 04/10/2005 (Begin) Check for the Shipdate Range
    DO CASE
      CASE !EMPTY(STDATE) AND !EMPTY(NDDATE)
        ldDatRange ="BETWEEN(INVHDR.INVDATE,STDATE,NDDATE)"
      CASE !EMPTY(STDATE) AND EMPTY(NDDATE)
        ldDatRange ="INVHDR.INVDATE>=STDATE"
      CASE EMPTY(STDATE) AND EMPTY(NDDATE)
        ldDatRange = ''
    ENDCASE
    IF EMPTY(ldDatRange)
      m.Amount=InvHdr.TotalChg
    ENDIF
    IF !EMPTY(ldDatRange) 
      IF &ldDatRange
    *B126530,1 NNA (End)
    
        m.Amount=InvHdr.TotalChg

    *B126530,1 NNA (Begin)
      ENDIF
    ENDIF
    *B126530,1 (End)
  ELSE
    *B037936,1 NNA 05/04/2004 (Begin) if the Rank by book , Get the Rep from OrdHdr
    m.Rep   =OrdHdr.Rep1
    *B037936,1 NNA (End)

    *B126530,1 NNA 04/10/2005 (Begin) Get the Booked and Open Amount from the ordline File and subtract the Cancelled Amount
    *m.Amount=OrdHdr.Bookamt
    IF SEEK('O'+ORDHDR.ORDER,'ORDLINE')
      ldDatRange =IIF(!EMPTY(STDATE) OR !EMPTY(NDDATE)," FOR BETWEEN(ORDLINE.Complete,STDATE,NDDATE)",'')
      SELECT ORDLINE
      SET RELATION TO
      SET RELATION TO Ordline.cordtype+ Ordline.order+STR( Ordline.lineno,6) INTO Ordcanln ADDITIVE
      SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order &ldDatRange
        m.Amount=m.Amount + (Ordline.TotBook*Ordline.Price)
        IF SEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
          SELECT OrdCanln
          SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
            m.Amount=m.Amount - (OrdCanln.TotQty*Ordline.Price)
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF    
    *B126530,1 NNA (End)
    
  ENDIF  
*B037936,1 NNA (Begin) If the User select to Rank By Both I'll get Data according to the file that I have
ELSE
  IF lnAmtBoth = 1
    m.Rep    =InvHdr.Rep1 

    *B126530,1 NNA 04/10/2005 (Begin) Check for the Shipdate Range
    IF STDATE<>{} OR NDDATE<>{}
      IF BETWEEN(INVHDR.INVDATE,STDATE,NDDATE)
    *B126530,1 NNA (End)

        m.Amount=InvHdr.TotalChg
  
    *B126530,1 NNA (Begin)
      ENDIF
    ELSE
      m.Amount=InvHdr.TotalChg
    ENDIF
    *B126530,1 NNA (End)

  ENDIF
  IF lnAmtBoth = 2
    m.Rep    =OrdHdr.Rep1  

    *B126530,1 NNA 04/10/2005 (Begin) Get the Booked and Open Amount from the ordline File and subtract the Cancelled Amount
    *m.AMTBOOK=OrdHdr.Bookamt    
    IF SEEK('O'+ORDHDR.ORDER,'ORDLINE')
      ldDatRange =IIF(!EMPTY(STDATE) OR !EMPTY(NDDATE),'FOR BETWEEN(ORDLINE.Complete,STDATE,NDDATE)','')
      SELECT ORDLINE
      SET RELATION TO
      SET RELATION TO Ordline.cordtype+ Ordline.order+STR( Ordline.lineno,6) INTO Ordcanln ADDITIVE
      SCAN REST WHILE cOrdtype+Order = 'O'+ Ordhdr.order &ldDatRange
        m.Amtbook=m.Amtbook + (Ordline.TotBook*Ordline.Price)
        IF ORDHDR.STATUS = 'O'
          m.Amtopen=m.Amtopen + (Ordline.TotQty*Ordline.Price)
        ENDIF
        IF SEEK(Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6),'Ordcanln')
          SELECT OrdCanln
          SCAN REST WHILE Cordtype+ Order+STR(lineno,6)= Ordline.cordtype+ Ordline.order+STR(Ordline.lineno,6)
            m.Amtbook=m.Amtbook - (OrdCanln.TotQty*Ordline.Price)
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF    
    *B126530,1 NNA (End)
  ENDIF  
ENDIF  
*B037936,1 NNA (End)
*-- end of lfGetMem.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*!B03793,1
FUNCTION lfwRepWhen
*--DECLARE array to store order status and make mover
  DECLARE laRpSource[5],laRpTarget[1]
  STORE 'Open'      TO laRpSource[1]
  STORE 'Hold'      TO laRpSource[2]
  STORE 'Bid'       TO laRpSource[3]
  STORE 'Complete'  TO laRpSource[4]
  STORE 'Cancelled' TO laRpSource[5]

  lcRpStatus = 'OHBCX'

*-- END lfwRepWhen 

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*!B03793,1
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpOrdSt  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpOrdSt = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpOrdSt = lcRpOrdSt +IIF(laRpTarget[lnI] = 'Open'     , 'O' ,;
                           IIF(laRpTarget[lnI] = 'Hold'     , 'H' ,;
                           IIF(laRpTarget[lnI] = 'Bid'      , 'B' ,;
                           IIF(laRpTarget[lnI] = 'Complete' , 'C' ,;
                           IIF(laRpTarget[lnI] = 'Cancelled', 'X' ,'')))))
  ENDFOR  && End Loop to make Status expression.
ENDIF

lcRpOrdSt = IIF(EMPTY(lcRpOrdSt),'OHB',ALLTRIM(lcRpOrdSt))

*-- Compare current selected status with old value to rise change status flag.
*-- if length of current selected status differ from previous length 

IF LEN(lcOldStat) != LEN(lcRpOrdSt) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpOrdSt)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [End]
*-- End of lfvOStatus.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : NADER NABIL (NNA)
*! Date      : 04/07/2005
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
*!B126530,1
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo
ldFrom = STDATE
LDTO   = NDDATE
lcTitle = 'Date range'
DO DateRng.Spx
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : NADER NABIL (NNA)
*! Date      : 04/07/2005
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
*!B126530,1
FUNCTION lfvpbOk
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  STDATE = ldFrom
  NDDate = ldTo
  CLEAR READ
ENDIF

