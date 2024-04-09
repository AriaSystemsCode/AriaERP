*:****************************************************************
*: Program file  : SMCNAMS
*: Program desc. : Data Conversion From AIMS to Aria27.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : System Manager - [SM]
*: Developer     : ABDOU ELGENDI -  [ABD]
*: Date          : 10/26/2003
*: Tracking Job Number : C119954,1
*:****************************************************************
*: Calls         : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : ..
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C119954,1.
*:****************************************************************
*:Modifications  :
*:B121775,1 ASH 02/24/04 Fix the bug of not converting the ARHIST file.
*:****************************************************************
*:

*-- Message to the user to make sure that He/She will convert the Aims data.
IF gfModalGen("TRM00000B00006","DIALOG",.F.,.F.,;
              'Are you sure you want to convert Aims data to company ' +gcAct_Comp +' .') = 2
  RETURN
ENDIF 


lcOldPath = FULLPATH('')
DO WHILE .T.
  lcAms_Dir = GETDIR('','Select Aims Directory')
  lcAms_Dir = ALLTRIM(lcAms_Dir)
  lcAms_Data = lcAms_Dir+'Data1\'
  lcAms_Data = ALLTRIM(lcAms_Data)
  
  *-- Exit If user press cancel.
  IF EMPTY(lcAms_Dir)
    EXIT
  ELSE
    *-- Check on the style file just any file for refremce
    IF FILE(lcAms_Data+'INVOICE.DBF')
      EXIT
    ELSE
      = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select Aims data files folder, cannot proceed.')
      STORE '' TO lcAms_Dir , lcAms_Data
    ENDIF
    
  ENDIF  
ENDDO
SET DEFA TO &lcOldPath

IF EMPTY(lcAms_Dir)
  RETURN
ENDIF



lcCurrCode = gcBaseCurr      && Variable to hold currency code.
lnExRate   = 1               && Variable to hold exchange rate.
lnCurrUnit = 1               && Variable to hold exchange rate.
lcUntSin   = '/'             && Variable to hold unit sign.
lcExRSin   = '/'             && Variable to hold exchange rate sign.

*ash1
USE (gcDataDir+'WAREHOUS') IN 0 
GO TOP
lcWare = WAREHOUS.cWarecode

*-- Convert color file.
DO lpCon_Colr


*-- Convert codes file.
DO lpCon_Cods


*-- Convert Credit - Debit
DO lpCon_DbCr

*-- Convert Invoice Header and Invoice line.
DO lpCon_INV

DO lpCon_INH

*-- Conver the Customer file.
DO lpConv_Cus

*-- CONVERT THE CUSTOMER AGING DATA
DO lpConv_Age

*-- CONVERT OLD Scale file into new format.
Do lpCon_Scal

*-- CONVERT OLD style FIILE INTO NEW FORMAT
DO lpCon_Styl

*-- CONVERT VENDOR FILE
DO lpCon_Vend

*-- CONVERT OLD SALESREP FIILE INTO NEW FORMAT
DO lpCon_Sals

*- CONVERT OLD SALESREP COMMATION FIILE INTO NEW FORMAT
DO lpCon_SlCm

*-- CONVERT OLD ORDER HEADER FIILE INTO NEW FORMAT
DO lpCon_Ordh

*-- CONVERT OLD ORDER LINE FILE INTO NEW FORMAT
DO lpCon_OrdL

*-- Convert Piktkt File.
DO lpCon_Pikt

*-- Convert Rahdr file.
DO lpCon_Rah

*-- Convert Rahlin file.
DO lpCon_RaL

*-- Create beginnig balance in StyInvJl file
DO lpCon_StyJ

*- Message Text   :- Data was converted successfully from the Aims System to Aria 2.7 system.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
lcMessage = 'Data was converted successfully from the Aims System to Aria 2.7 system.'
= gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)

RETURN
*-- End OF Convertion.
*:****************************************************************
PROCEDURE lpCon_DbCr

SET EXCLUSIVE OFF
USE (lcAms_Data+'INVOICE') IN 0 

SET EXCLUSIVE ON
USE (gcDataDir+'DEBIT') IN 0 EXCLUSIVE
SELECT DEBIT
ZAP
USE

USE (gcDataDir+'CREDIT') IN 0 EXCLUSIVE
SELECT CREDIT
ZAP
USE

SET EXCLUSIVE OFF
USE (gcDataDir+'DEBIT')  IN 0 SHARED
USE (gcDataDir+'CREDIT') IN 0 SHARED
USE (gcDataDir+'ARHIST') IN 0 SHARED
lcAccount = ' '
SELECT INVOICE
SET ORDER TO 1
SCAN
  Wait Window 'Converting Invoice File, Invoice # : '+ Invoice + ' Please Wait.' NOWAIT
  *B121775,1 ASH 02/24/04 (Begin) Don't skip the record of 0 balance to be saved in the ARHIST file.
  *IF EMPTY(ACCOUNT) .OR. SEQ<>"0" .OR. BALANCE = 0
  IF EMPTY(ACCOUNT) 
  *B121775,1 ASH 02/24/04 (End)
    LOOP
  ENDIF
  *B121775,1 ASH 02/24/04 (Begin) Fix the bug of not converting the ARHIST file.
  IF BALANCE = 0 AND TRANCD $ '13' AND TotalChg <> 0
    IF Account <> lcAccount 
      lcHisSeq  = gfsequence("HISTORY")
      lcRepBat  = gfsequence("BATCH")  
      lcAccount = Account 
    ENDIF
    DO CASE
      CASE TRANCD = '1'
        lcTranTyp = '1'
        lcDesc    = 'INVOICE SALES ORDER'
      CASE TRANCD = '3'
        lcTranTyp = '4'
        lcDesc    = 'Invoice Payment'
    ENDCASE
    SELECT ARHIST
    APPEND BLANK
    REPLACE Account   WITH INVOICE.ACCOUNT ,;
            History   WITH lcHisSeq        ,;
            TranType  WITH lcTranTyp       ,;
            Tran      WITH Invoice.Invoice ,;
            TranDate  WITH Invoice.InvDate ,;
            Batch     WITH IIF(INVOICE.TranCd='3',lcRepBat,'') ,;
            Amount    WITH IIF(INVOICE.TranCd='3',Invoice.TotalChg*-1,Invoice.TotalChg),;
            Store     WITH INVOICE.Store   ,;
            REFERENCE WITH INVOICE.CUSTPO  ,;
            Desc      WITH lcDesc          ,;
            TotDb     WITH Invoice.TotalChg,;
            TotCr     WITH Invoice.TotalChg*-1,;
            HistDate  WITH DATE()          ,;
            cCurrCode WITH 'USD'           ,;
            nCurrUnit WITH 1               ,;
            nExRate   WITH 1               

    =gfAdd_Info('ARHIST')
    LOOP
  ENDIF  
  *B121775,1 ASH 02/24/04 (End)
  XINVDATE = INVDATE
  XDUEDATE = IIF(SUBSTR(INVOICE,1,1)='2',XINVDATE+30,XINVDATE)

  IF BALANCE < 0
    SELECT CREDIT 
    APPEND BLANK

    *-- Credit Code.
    lcCrditCod = lfEqv_Code('CCREDITCOD',INVOICE.REASON,'B')
    
    REPLACE ACCOUNT    WITH INVOICE.ACCOUNT ,;
            STORE      WITH INVOICE.STORE   ,;
            TRAN       WITH INVOICE.INVOICE ,;
            CCREDITCOD WITH lcCrditCod      ,;
            TRANTYPE   WITH '6'             ,;
            TRANDATE   WITH XINVDATE        ,;
            DESC       WITH IIF(INVOICE.INVOICE='000000' ,'UNAP. CREDIT',INVOICE.DESC),;
            REFERENCE  WITH INVOICE.CUSTPO ,;
            AMOUNT     WITH INVOICE.BALANCE,;
            cFacCode   WITH INVOICE.FACTOR ,;
            DPOStDate  WITH XINVDATE       ,;
            cCurrCode  WITH gcBaseCurr     ,;
            nCurrunit  WITH 1              ,;
            Nexrate    WITH 1.0000 
         

    =gfAdd_Info('CREDIT')
  ELSE
    SELECT DEBIT
    APPEND BLANK
    *-- Debit Code
    lcDebitCod = lfEqv_Code('TRANCODE  ',INVOICE.REASON,'A')
    
    REPLACE ACCOUNT  WITH INVOICE.ACCOUNT,;
            STORE    WITH INVOICE.STORE  ,;
            TRAN     WITH INVOICE.INVOICE,;
            TRANCODE WITH lcDebitCod     ,;
            TRANTYPE WITH '1'            ,;
            TRANDATE WITH XINVDATE       ,;
            DESC     WITH IIF(SUBSTR(INVOICE.INVOICE,1,1) ='2'  .AND. ( INVOICE.BALANCE = INVOICE.FCTRTOT ) ,'INVOICE',;
                     'INVOICE-A'),;
            REFERENCE WITH INVOICE.CUSTPO ,;
            AMOUNT    WITH INVOICE.SHIPAMT,;
            cFacCode  WITH INVOICE.FACTOR ,;
            DUEDATE   WITH XDUEDATE       ,;
            DPOStDate WITH XINVDATE       ,;
            cCurrCode WITH gcBaseCurr     ,;
            nCurrunit WITH 1              ,;
            Nexrate   WITH 1.0000 
         
    =gfAdd_Info('DEBIT')
  ENDIF
  SELECT INVOICE     
ENDSCAN

USE IN INVOICE
USE IN DEBIT
USE IN CREDIT

WAIT CLEAR
*-- End OF lpCon_DbCr
*:****************************************************************
PROCEDURE lpCon_INV

SET EXCLUSIVE OFF
USE (lcAms_Data+'INVLINE') IN 0 Alias AmsInv

SET EXCLUSIVE ON
USE (gcDataDir+'INVLINE') IN 0 EXCLUSIVE ALIAS ARIAInv
SELECT ARIAInv
ZAP
USE


SET EXCLUSIVE OFF
USE (gcDataDir+'INVLINE') IN 0 SHARED ALIAS ARIAInv

USE (lcAms_Data+'InvHdr') ORDER First IN 0 

SELECT AMSInv
SCAN
  Wait Window 'Converting Invoice Line File, Invoice # : '+ Invoice + ' Please Wait.' NOWAIT
  SELECT ARIAInv
  APPEND BLANK
  =SEEK(AmsInv.INVOICE,'InvHdr')
  lcSeason= ''
  IF !EMPTY(InvHdr.Season)
    lcSeason = lfUpSeaDiv('SEASON'+InvHdr.Season)
  ENDIF
  
  REPLACE INVOICE    WITH AmsInv.INVOICE     ,;
          INVDATE    WITH AmsInv.INVDATE     ,;
          LINENO     WITH VAL(AmsInv.LINENO) ,;
          SHIPMENT   WITH 1                 ,;
          ORDER      WITH AmsInv.ORDER       ,;
          ACCOUNT    WITH AmsInv.ACCOUNT     ,;
          STORE      WITH AmsInv.STORE       ,;
          STYLE      WITH PADR(AmsInv.STYLE,12)+'-'+ PADR(AmsInv.COLOR,6),;
          SCALE      WITH AmsInv.SCALE       ,;
          PRICE      WITH AmsInv.PRICE       ,;
          QTY1       WITH AmsInv.SHP0        ,;
          QTY2       WITH AmsInv.SHP1        ,;
          QTY3       WITH AmsInv.SHP2        ,;
          QTY4       WITH AmsInv.SHP3        ,;
          QTY5       WITH AmsInv.SHP4        ,;
          QTY6       WITH AmsInv.SHP5        ,;
          QTY7       WITH AmsInv.SHP6        ,;
          QTY8       WITH AmsInv.SHP7        ,;
          TOTQTY     WITH AmsInv.SHPTOT      ,;
          Gros_Price WITH AmsInv.PRICE       ,;
          Disc_Pcnt  WITH 0                  ,;
          Gl_Sales   WITH 'DEFDEF'           ,;
          GL_Cost    WITH 'DEFDEF'           ,;
          SEASON     WITH lcSeason           ,;
          cWareCode  WITH lcWare

  =gfAdd_Info('ARIAInv')
  SELECT AMSInv
  
ENDSCAN

USE IN ARIAInv
USE IN AmsInv
USE IN INVHDR
WAIT CLEAR

*-- End OF lpCon_INV
*:****************************************************************
PROCEDURE lpCon_INH

SET EXCLUSIVE ON
USE (gcDataDir+'INVHDR') IN 0 EXCLUSIVE
SELECT INVHDR
ZAP
USE

USE (gcDataDir+'INVHDR') IN 0 SHARED

SET EXCLUSIVE OFF
USE (lcAms_Data+'INVOICE') IN 0 
SELECT INVOICE

SCAN FOR (TRANCD='1' AND DESC='INVOICE')
  SELECT INVHDR
  Wait Window 'Converting Invoice Header File, Invoice # : '+ Invoice + ' Please Wait.' NOWAIT  
  APPEN BLANK
   
  XTERMS  = lfEqv_Code('CTERMCODE ',Invoice.TERMS,'T')
  ldDate=Invoice.InvDate
  ldDueD=Invoice.Duedate

  lcSeason = ''
  IF !EMPTY(Invoice.SEASON)
    lcSeason = lfUpSeaDiv('SEASON'+Invoice.SEASON)
  ENDIF

  lcDivison = ''
  IF !EMPTY(Invoice.DIVISION)
    lcDivison = lfUpSeaDiv('Division'+Invoice.DIVISION)
  ENDIF

  lcShipVia  = lfEqv_Code('SHIPVIA   ',Invoice.ShipVia,'T')
  *-- Get the default Spinst. 
  lcSpInst   = lfEqv_Code('SPCINST   ','','I')


  
  REPL INVOICE   WITH Invoice.INVOICE  ,;
       INVDATE   WITH ldDate           ,;
       SHIPDATE  WITH ldDate           ,;
       DueDate   WITH ldDueD           ,;
       dpostDate WITH ldDate           ,;
       STATUS    WITH 'C'              ,;
       ORDER     WITH Invoice.ORDER    ,;
       ACCOUNT   WITH Invoice.ACCOUNT  ,;
       cFacCode  WITH Invoice.FACTOR   ,;
       STORE     WITH Invoice.STORE    ,;
       UPSZONE   WITH ''               ,;
       SEASON    WITH lcSeason         ,;
       cDIVISION WITH lcDivison        ,;
       ctermCode WITH XTERMS           ,;
       SHIPVIA   WITH lcShipVia        ,;
       SPCINST   WITH lcSpInst         ,;
       REP1      WITH Invoice.SALESREP1,;
       REP2      WITH Invoice.SALESREP2,;
       COMM1     WITH Invoice.COMMPCNT1,;
       COMM2     WITH Invoice.COMMPCNT2,;
       COMMAMT1  WITH Invoice.ShipAmt*Invoice.COMMPCNT1,;
       COMMAMT2  WITH Invoice.ShipAmt*Invoice.COMMPCNT2,;
       CUSTPO    WITH Invoice.CUSTPO  ,;
       DISCOUNT  WITH Invoice.DISCOUNT,;
       SHIP      WITH Invoice.SHIP    ,;
       SHIPAMT   WITH Invoice.SHIPAMT ,;
       FREIGHT   WITH Invoice.FREIGHT ,;
       TOTALCHG  WITH Invoice.SHIPAMT+Invoice.FREIGHT+Invoice.DISCOUNT,;
       cWareCode WITH lcWare          ,;
       Weight    WITH Invoice.Weight  ,;
       CARTONS   WITH 1

  =gfAdd_Info('INVHDR')
  SELECT INVOICE
ENDSCAN
Wait CLEAR
USE IN INVOICE
USE IN INVHDR

*-- End OF lpCon_INH 
*:****************************************************************
PROCEDURE lpConv_Cus

SET EXCLUSIVE OFF
USE (lcAms_Data+'CUSTOMER') ALIAS OLD IN 0
SELECT OLD
SET ORDER TO TAG Account

SET EXCLUSIVE ON
USE (gcDataDir+'CUSTOMER') ALIAS NEW IN 0
SELECT NEW
ZAP
USE

USE (gcDataDir+'ARCUSHST') IN 0
SELECT ARCUSHST
ZAP
USE

SET EXCLUSIVE OFF
USE (gcDataDir+'CUSTOMER') ALIAS NEW IN 0
USE (gcDataDir+'ARCUSHST') IN 0 ORDER Acthst

SELE OLD
LOCATE
WAIT WINDOW 'Converting Customer file.' NOWAIT
SCAN
  WAIT WINDOW 'Converting Customer File, Account # : '+ Account + ' Please Wait.' NOWAIT
  SELE OLD
  IF EMPTY(ALLTRIM(Account)) OR LEN(ALLTRIM(Account)) <> 5
    LOOP
  ENDIF

  SELECT NEW
  APPE BLANK
  XR= RLOCK()
  REPLACE ACCOUNT   WITH OLD.ACCOUNT
  DO CASE
    CASE OLD.TYPE = '1'
      REPLACE TYPE        WITH 'M'         ,;
              BillTo      WITH 'M'         ,;
              BTNAME      WITH OLD.NAME    ,;
              cCont_Code  WITH 'USA'       ,;
              cCont_Code2 WITH 'USA'       ,;
              cAddress1   WITH OLD.ADDR1   ,;
              cAddress12  WITH OLD.ADDR1   ,;
              cAddress2   WITH OLD.ADDR2   ,;
              cAddress22  WITH OLD.ADDR2   ,;
              cAddress3   WITH OLD.CITY    ,;
              cAddress32  WITH OLD.CITY    ,;
              cAddress4   WITH OLD.STATE   ,;
              cAddress42  WITH OLD.STATE   ,;
              cAddress5   WITH OLD.ZIP     ,;
              cAddress52  WITH OLD.ZIP     ,;
              cAddress6   WITH 'USA'       ,;
              cAddress62  WITH 'USA'       ,;
              STNAME      WITH OLD.NAME    ,;
              cFacCode    WITH OLD.FACTOR  ,; 
              FACTACCT    WITH OLD.FACTACCT,;
              UPSZONE     WITH OLD.UPSZONE ,;
              BUYER       WITH OLD.BUYER   ,;
              KEEPER      WITH OLD.KEEPER  ,;
              NOTE        WITH OLD.NOTE    ,;
              SALESREP    WITH OLD.SALESREP,;
              COMM        WITH OLD.COMMPCNT,;
              PRICELVL    WITH 'A' 
  
      *-- Update the Customer history file with the next and current and last year.
      IF !SEEK(OLD.ACCOUNT,'ARCUSHST') 
        INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (OLD.ACCOUNT,STR(VAL(gcCurrYear)-1,4))
        INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (OLD.ACCOUNT,gcCurrYear)
        INSERT INTO ARCUSHST (Account,cFisFYear) VALUES (OLD.ACCOUNT,STR(VAL(gcCurrYear)+1,4))
      ENDIF

    CASE OLD.TYPE = '2'
      REPLACE TYPE      WITH 'S'        ,;
              STORE     WITH OLD.STORE  ,;
              STNAME    WITH OLD.NAME   ,;
              cAddress1 WITH OLD.ADDR1  ,;
              cAddress2 WITH OLD.ADDR2  ,;
              cAddress3 WITH OLD.CITY   ,;
              cAddress4 WITH OLD.STATE  ,;
              cAddress5 WITH OLD.ZIP    ,;
              cAddress6 WITH 'USA'      ,;
              SALESREP  WITH OLD.SALESREP,;
              COMM      WITH OLD.COMMPCNT

      DO CASE
        CASE OLD.BILLADDR='M'   .OR. EMPTY(OLD.BILLADDR)
        ** SAVE THE RECORD NO OF THE STORE
        SELECT OLD
        SAVREC   = RECNO()
        XACCOUNT = ACCOUNT
        XSTORE   = SPACE(8)
 
        * FIND THE MASTER ACCOUNT
        SEEK XACCOUNT+XSTORE+'1'
        SELECT NEW

        REPLACE BTNAME   WITH OLD.NAME     ,;
                cAddress12  WITH OLD.ADDR1 ,;
                cAddress22  WITH OLD.ADDR2 ,;
                cAddress32  WITH OLD.CITY  ,;
                cAddress42  WITH OLD.STATE ,;
                cAddress52  WITH OLD.ZIP   ,;
                cAddress62  WITH 'USA'     ,;
                cFacCode WITH OLD.FACTOR   ,;
                FACTACCT WITH OLD.FACTACCT ,;
                UPSZONE  WITH OLD.UPSZONE  ,;
                BUYER    WITH OLD.BUYER    ,;
                KEEPER   WITH OLD.KEEPER   ,;
                NOTE     WITH OLD.NOTE     ,;
                SALESREP WITH OLD.SALESREP ,;
                COMM     WITH OLD.COMMPCNT ,;
                PRICELVL WITH 'A'

          SELECT OLD
          GOTO SAVREC


        CASE OLD.BILLADDR='S'
          REPLACE BTNAME   WITH OLD.NAME     ,;
                  cAddress12  WITH OLD.ADDR1 ,;
                  cAddress22  WITH OLD.ADDR2 ,;
                  cAddress32  WITH OLD.CITY  ,;
                  cAddress42  WITH OLD.STATE ,;
                  cAddress52  WITH OLD.ZIP   ,;
                  cAddress62  WITH 'USA'     ,;
                  cFacCode WITH OLD.FACTOR   ,;
                  FACTACCT WITH OLD.FACTACCT ,;
                  UPSZONE  WITH OLD.UPSZONE  ,;
                  BUYER    WITH OLD.BUYER    ,;
                  KEEPER   WITH OLD.KEEPER   ,;
                  NOTE     WITH OLD.NOTE     ,;
                  SALESREP WITH OLD.SALESREP ,;
                  COMM     WITH OLD.COMMPCNT ,;
                  PRICELVL WITH 'A'


      ENDCASE
    CASE OLD.TYPE = 'F'
      SELECT OLD
      LOOP
  ENDCASE
  
  lcClass    = lfEqv_Code('CLASS     ',OLD.CLASS,'C')
  lcTermCode = lfEqv_Code('CTERMCODE ',OLD.TERMS,'T')
  lcShipVia  = lfEqv_Code('SHIPVIA   ',OLD.ShipVia,'T')
  lcSpInst   = lfEqv_Code('SPCINST   ',OLD.SPCINST,'I')
  
  SELECT NEW
  
  lcDivison = ''
  IF !EMPTY(OLD.DIVISION)
    lcDivison = lfUpSeaDiv('Division'+OLD.DIVISION)
  ENDIF
  lcRegion = lfEqv_Code('REGION    ',OLD.MKTREGION,'R')
  
  REPLACE cDivision  WITH lcDivison     ,;
          REGION     WITH lcRegion      ,;
          CLASS      WITH lcClass       ,;
          cTermCode  WITH lcTermCode    ,;
          STATUS     WITH OLD.STATUS    ,;
          SHIPVIA    WITH lcShipVia     ,;
          SPCINST    WITH lcSpInst      ,;
          cINSUR     WITH OLD.INSURCD   ,;
          PRIORITY   WITH OLD.PRIORITY  ,; 
          COUNTRY    WITH OLD.COUNTRY   ,;
          DUNS       WITH OLD.DUNS      ,;
          DUNSRTG    WITH OLD.DUNSRATE  ,;
          UPS_INCR   WITH OLD.UPSPLUS   ,; 
          PRNT_STATM WITH OLD.STMT      ,;
          DISC       WITH OLD.DISCPCNT
  lcNum = '0123456789'
  lcPhone=''
  IF !EMPTY(ALLTRIM(OLD.PHONE))
    FOR I = 1 TO LEN(ALLTRIM(OLD.PHONE))
       IF SUBSTR(OLD.PHONE,I,1) $ lcNum
         lcPhone = lcPhone + SUBSTR(OLD.PHONE,I,1)
       ENDIF
    ENDFOR
  ENDIF
  REPLACE PHONE1     WITH lcPhone     ,;
          NETBAL     WITH OLD.TotBal  ,;
          TERCURRENT WITH OLD.TCURR   ,;
          TERAGE30   WITH OLD.TAGE30  ,;
          TERAGE60   WITH OLD.TAGE60  ,;
          TERAGE90   WITH OLD.TAGE90  ,;
          TERAGE120  WITH OLD.TAGE120 ,;
          AGEDATE    WITH OLD.AGEDATE ,;
          CRLIMIT    WITH OLD.CRLIMIT ,;
          BillTo     WITH OLD.BILLADDR,;
          Fax        WITH OLD.Fax
  UNLOCK
  =gfAdd_Info('NEW')
  SELECT OLD
ENDSCAN

WAIT CLEAR

USE IN NEW
USE IN OLD

*-- End OF lpConv_Cus
*****************************************************************
PROCEDURE lpConv_Age

SET EXCLUSIVE ON
USE (gcDataDir+'CUSTOMER') IN 0
SELECT CUSTOMER
SET ORDER TO TAG CUSTOMER
=SEEK('M')

SELECT 0
USE (gcDataDir+'CREDIT')
SET ORDER TO TAG CREDIT

SELECT 0
USE (gcDataDir+'DEBIT')
SET ORDER TO TAG DEBIT

WAIT WINDOW 'Converting the customer aging data.' NOWAIT
SELECT CUSTOMER
SCAN REST WHILE TYPE = 'M'
  WAIT WINDOW 'Converting the customer aging data, Account # :'+Account + ' Please Wait.' NOWAIT
  REPLACE AGEDATE    WITH DATE(),;
          CURRENT    WITH 0.00  ,;
          AGE30      WITH 0.00  ,; 
          AGE60      WITH 0.00  ,;
          AGE90      WITH 0.00  ,;
          AGE120     WITH 0.00  ,;
          TOTAGE     WITH 0.00  ,;
          OPENCR     WITH 0.00  ,;
          CHGBACK    WITH 0.00  ,;
          NETBAL     WITH 0.00  ,;
          TERCURRENT WITH 0.00  ,;
          TERAGE30   WITH 0.00  ,;
          TERAGE60   WITH 0.00  ,;
          TERAGE90   WITH 0.00  ,;
          TERAGE120  WITH 0.00
  STORE 0.00 TO XCURRENT,XAGE30,XAGE60,XAGE90,XAGE120,XOPENAR,XOPENCR,XCHGBACK
  STORE 0.00 TO YCURR,   YAGE30,YAGE60,YAGE90,YAGE120
  SELECT DEBIT
  IF SEEK(Customer.Account)
    SCAN WHILE Account = Customer.Account
      DAYS = DATE() - TRANDATE
      DO CASE
        CASE DAYS>=120
          XAGE120 = XAGE120 + AMOUNT
        CASE DAYS>=90
          XAGE90  = XAGE90  + AMOUNT
        CASE DAYS>=60
          XAGE60  = XAGE60  + AMOUNT
        CASE DAYS>=30
          XAGE30  = XAGE30  + AMOUNT
        OTHERWISE
          XCURRENT=XCURRENT + AMOUNT
      ENDCASE

      IF TRANTYPE = '3'
         XCHGBACK = XCHGBACK + AMOUNT
      ENDIF
      
      XDUEDATE = IIF(DUEDATE=CTOD('  /  /  '), TRANDATE+30, DUEDATE)
      DAYS     = DATE() - XDUEDATE
      DO CASE
        * 91+
        CASE DAYS>=91
          YAGE120 = YAGE120 + AMOUNT
        * +61 - 90
        CASE DAYS>=61
          YAGE90  = YAGE90  + AMOUNT
        * +31 - 60
        CASE DAYS>=31
          YAGE60  = YAGE60  + AMOUNT
        * +1 - 30
        CASE DAYS>=1
          YAGE30  = YAGE30  + AMOUNT
        * Current
        OTHERWISE
          YCURR   = YCURR   + AMOUNT
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT CREDIT
  IF SEEK(Customer.Account)
    SUM REST AMOUNT TO XOPENCR WHILE Account = Customer.Account
  ENDIF
  SELECT CUSTOMER
  REPLACE CURRENT    WITH XCURRENT       ,;
          AGE30      WITH XAGE30         ,;
          AGE60      WITH XAGE60         ,;
          AGE90      WITH XAGE90         ,;
          AGE120     WITH XAGE120        ,;
          TOTAGE     WITH CURRENT+AGE30+AGE60+AGE90+AGE120,;
          OPENCR     WITH XOPENCR        ,;
          CHGBACK    WITH XCHGBACK       ,;
          NETBAL     WITH TOTAGE + OPENCR,;
          TERCURRENT WITH YCURR          ,; 
          TERAGE30   WITH YAGE30         ,;
          TERAGE60   WITH YAGE60         ,;
          TERAGE90   WITH YAGE90         ,;
          TERAGE120  WITH YAGE120
  
ENDSCAN
WAIT CLEAR

USE IN CUSTOMER
USE IN DEBIT
USE IN CREDIT

*--- End OF lpConv_Age
*****************************************************************
PROCEDURE lpCon_Scal

USE (lcAms_Data+'Scale') ALIAS OLD IN 0

SET EXCLUSIVE ON
USE (gcDataDir+'Scale') ALIAS NEW IN 0
SELECT NEW
ZAP
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'Scale') ALIAS NEW IN 0

WAIT WINDOW 'Converting Scale file.' NOWAIT

SELECT OLD

GO TOP
SCAN
  WAIT WINDOW 'Converting Scale File, Scale #: '+ Scale + ' Please Wait.' NOWAIT
  lcType = IIF(!EMPTY(Prepak),'P','S')
  SELECT NEW
  APPEND BLANK
  REPLACE Type      WITH lcType        ,;
          Scale     WITH Old.Scale     ,;
          cScl_desc WITH Old.Desc      ,;
          Cnt       WITH Old.Count      
  IF lcType = 'S'          
    REPLACE Sz1     WITH Old.Size0   ,;
            Sz2     WITH Old.Size1   ,;
            Sz3     WITH Old.Size2   ,;
            Sz4     WITH Old.Size3   ,;
            Sz5     WITH Old.Size4   ,;
            Sz6     WITH Old.Size5   ,;
            Sz7     WITH Old.Size6   ,;
            Sz8     WITH Old.Size7      
  ELSE
    REPLACE Pp1     WITH Old.Pp0     ,;
            Pp2     WITH Old.Pp1     ,;
            Pp3     WITH Old.Pp2     ,;
            Pp4     WITH Old.Pp3     ,;
            Pp5     WITH Old.Pp4     ,;
            Pp6     WITH Old.Pp5     ,;
            Pp7     WITH Old.Pp6     ,;
            Pp8     WITH Old.Pp7     ,;
            PpTot   WITH Old.PpTot   ,;
            Prepak  WITH Old.Prepak
  ENDIF            
  REPLACE CNRFCODE1 WITH OLD.NRF0    ,;
          CNRFCODE2 WITH OLD.NRF1    ,;
          CNRFCODE3 WITH OLD.NRF2    ,;
          CNRFCODE4 WITH OLD.NRF3    ,;
          CNRFCODE5 WITH OLD.NRF4    ,;
          CNRFCODE6 WITH OLD.NRF5    ,;
          CNRFCODE7 WITH OLD.NRF6    ,;
          CNRFCODE8 WITH OLD.NRF7

  =gfAdd_Info('NEW')
  SELECT OLD          
ENDSCAN
WAIT CLEAR

USE IN OLD
USE IN NEW

*-- End OF lpCon_Scal
*:****************************************************************
PROCEDURE lpCon_Styl


USE (lcAms_Data+'STYLE') ALIAS OLD IN 0

SET EXCLUSIVE ON
USE (gcDataDir+'STYLE') ALIAS NEW IN 0
SELECT NEW
ZAP
USE

USE (gcDataDir+'STYDYE') IN 0
SELECT STYDYE
ZAP
USE
SET EXCLUSIVE OFF 


USE (gcDataDir+'STYLE') ALIAS NEW IN 0
USE (gcDataDir+'STYDYE')   IN 0
*USE (gcDataDir+'WAREHOUS') IN 0
*SELECT WAREHOUS
*LOCATE FOR cWarecode = 'DEFWHE'
*IF !FOUND()
*  APPEND BLANK
*  REPLACE CWARECODE   WITH 'DEFWHE',;
*           lflag      WITH .F.     ,;
*           caddress6  WITH 'USA'   ,;
*           lstyinv    WITH .T.     ,;
*           lmatinv    WITH .F.     ,;
*           cCont_Code WITH 'USA'
*  =gfAdd_Info('WAREHOUS')
*ENDIF


WAIT WINDOW 'Converting Style file.' NOWAIT

SELECT OLD
LOCATE

SCAN FOR !EMPTY(Style)
   Wait Window 'Converting Style File, Style #  '+ PADR(OLD.STYLE,12)+'-'+ PADR(OLD.COLOR,6) + ' Please Wait.' NOWAIT 
   SELECT NEW
   APPEND BLANK

  lcSeason = ''
  IF !EMPTY(OLD.SEASON)
    lcSeason = lfUpSeaDiv('SEASON'+OLD.SEASON)
  ENDIF

  lcDivison = ''
  IF !EMPTY(OLD.DIVISION)
    lcDivison = lfUpSeaDiv('Division'+OLD.DIVISION)
  ENDIF
   
   REPLACE STYLE      WITH PADR(OLD.STYLE,12)+'-'+ PADR(OLD.COLOR,6),;
           cstymajor WITH PADR(OLD.STYLE,19),;
           STATUS     WITH IIF(EMPTY(OLD.STATUS),'A',OLD.STATUS)    ,;
           cStyGroup  WITH OLD.GROUP   ,;
           SCALE      WITH OLD.SCALE   ,;
           PREPAK     WITH OLD.PREPAK  ,;
           CDIVISION  WITH lcDivison   ,;
           DESC       WITH OLD.DESC    ,;
           DESC1      WITH OLD.DESC    ,;
           nStyWeight WITH OLD.WEIGHT  ,;
           SEASON     WITH lcSeason    ,;
           cStyGrade  WITH '1'         ,;
           MAKE       WITH .F.         ,;
           NICOST1    WITH OLD.TOTCOST ,;
           TOTCOST    WITH OLD.TOTCOST ,;
           ORD1       WITH OLD.ORD0    ,;
           ORD2       WITH OLD.ORD1    ,;
           ORD3       WITH OLD.ORD2    ,;
           ORD4       WITH OLD.ORD3    ,;
           ORD5       WITH OLD.ORD4    ,;
           ORD6       WITH OLD.ORD5    ,;
           ORD7       WITH OLD.ORD6    ,;
           ORD8       WITH OLD.ORD7    ,;
           TOTORD     WITH OLD.ORDTOT  ,;
           WIP1       WITH OLD.WIP0    ,;
           WIP2       WITH OLD.WIP1    ,;
           WIP3       WITH OLD.WIP2    ,;
           WIP4       WITH OLD.WIP3    ,;
           WIP5       WITH OLD.WIP4    ,;
           WIP6       WITH OLD.WIP5    ,;
           WIP7       WITH OLD.WIP6    ,;
           WIP8       WITH OLD.WIP7    ,;
           TOTWIP     WITH OLD.WIPTOT  ,;
           STK1       WITH OLD.STK0    ,;
           STK2       WITH OLD.STK1    ,;
           STK3       WITH OLD.STK2    ,;
           STK4       WITH OLD.STK3    ,;
           STK5       WITH OLD.STK4    ,;
           STK6       WITH OLD.STK5    ,;
           STK7       WITH OLD.STK6    ,;
           STK8       WITH OLD.STK7    ,;
           TOTSTK     WITH OLD.STKTOT 
 
  REPLACE  ALO1     WITH OLD.ALO0    ,;
           ALO2     WITH OLD.ALO1    ,;
           ALO3     WITH OLD.ALO2    ,;
           ALO4     WITH OLD.ALO3    ,;
           ALO5     WITH OLD.ALO4    ,;
           ALO6     WITH OLD.ALO5    ,;
           ALO7     WITH OLD.ALO6    ,;
           ALO8     WITH OLD.ALO7    ,;
           TOTALO   WITH OLD.ALOTOT  ,;
           SHP1     WITH OLD.SHP0    ,;
           SHP2     WITH OLD.SHP1    ,;
           SHP3     WITH OLD.SHP2    ,;
           SHP4     WITH OLD.SHP3    ,;
           SHP5     WITH OLD.SHP4    ,;
           SHP6     WITH OLD.SHP5    ,; 
           SHP7     WITH OLD.SHP6    ,;
           SHP8     WITH OLD.SHP7    ,;
           TOTSHP   WITH OLD.SHPTOT  ,;
           PRICEA   WITH OLD.STDPRICE,;
           MARKA    WITH OLD.MARKUP  ,;
           SOLDOUT  WITH OLD.SOLDOUT ,;
           DMGQTY   WITH OLD.DAMAGE  ,;
           FABRIC   WITH OLD.FABRIC1 ,;
           lInvsty  WITH .T.         ,;
           lDetCost WITH .F.         ,;
           Commission WITH .T.       ,;
           cDye_Flg   WITH 'N'       ,;
           Link_Code  WITH 'DEFDEF'  ,;
           cPricecur  WITH lcCurrCode,;
           cDutyCur   WITH lcCurrCode,;
           nCurrRate  WITH lnExRate  ,;
           nTaxBreak  WITH lnCurrUnit,;
           cSlsglLink WITH 'DEF'     ,;
           cDefWare   WITH lcWare  
              
   =gfAdd_Info('NEW')
   SCATTER MEMVAR
   SELECT STYDYE
   APPEND BLANK
   GATHER MEMVAR
   *--Update Differencies
   =RLOCK() 
   REPLACE CWARECODE  WITH lcWare  ,;
           Gl_Link    WITH 'DEFDEF',;
           Dyelot     WITH SPACE(10)
   UNLOCK
  SELECT OLD
ENDSCAN
WAIT CLEAR

USE IN  OLD
USE IN  NEW
USE IN  STYDYE
USE IN  WAREHOUS

*-- End OF lpCon_Styl
*:****************************************************************
PROCEDURE lpCon_Vend

USE (lcAms_Data+'VENDOR') ALIAS OLD IN 0
SET EXCLUSIVE ON

USE (gcDataDir+'apvendor') ALIAS NEW IN 0
SELECT NEW
ZAP
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'apvendor') ALIAS NEW IN 0
USE (gcDataDir+'CONTACT') ORDER Contact IN 0 

SELE OLD
LOCATE
WAIT WINDOW 'Converting Vendor file.' NOWAIT
SCAN
  Wait Window 'Converting Vendor File, Vendor #  '+ OLD.VENDOR + ' Please Wait.' NOWAIT 
  SELECT NEW
  APPEND BLANK
  REPLACE cvendcode  WITH OLD.VENDOR ,;
          cVenComp   WITH OLD.NAME   ,;
          cCont_Code WITH 'USA'       ,;          
          cAddress1  WITH OLD.ADDR1   ,;
          cAddress2  WITH OLD.ADDR2   ,;
          cAddress3  WITH OLD.CITY    ,;
          cAddress4  WITH OLD.STATE   ,;
          cAddress5  WITH OLD.ZIP     ,;
          cAddress6  WITH 'USA'       ,;
          cVenPmeth  WITH 'P'         ,;
          cVenPrior  WITH "1"         ,;
          cCurrCode  WITH gcBaseCurr
          
  lcNum = '0123456789'
  lcPhone=''
  FOR I = 1 TO LEN(ALLTRIM(OLD.PHONE))
    IF SUBSTR(OLD.PHONE,I,1) $ lcNum
      lcPhone = lcPhone + SUBSTR(OLD.PHONE,I,1)
    ENDIF
  ENDFOR
  lcTermCode = lfEqv_Code('CTERMCODE ',OLD.TERMS,'T')
  REPLACE cPhoneNo  WITH lcPhone     ,;
          ctermCode WITH lcTermCode  ,;
          cVenComnt WITH OLD.COMMENT ,;
          cVenCont  WITH OLD.CONTACT
  
  *-- Add Contact into Contact file for the vendor.
  IF !EMPTY(OLD.CONTACT) .AND. !SEEK('V'+PADR(OLD.VENDOR,8)+OLD.CONTACT,'CONTACT')
    SELECT CONTACT
    APPEND BLANK
    REPLACE cconttype  WITH 'V',;
            ccont_id   WITH PADR(OLD.VENDOR,8),;
            contact    WITH OLD.CONTACT,;
            cCont_code WITH OLD.COUNTRY
    SELECT NEW
  ENDIF
          
  IF TYPE('OLD.FAX')='C' AND !EMPTY(OLD.FAX)
    lcFax=''
    FOR I = 1 TO LEN(ALLTRIM(OLD.FAX))
      IF SUBSTR(OLD.FAX,I,1) $ lcNum
        lcFax = lcFax + SUBSTR(OLD.FAX,I,1)
      ENDIF
    ENDFOR
    REPLACE cFaxNo WITH lcFax
  ENDIF
   =gfAdd_Info('NEW')  
  SELE OLD
ENDSCAN
WAIT CLEAR

USE IN OLD
USE IN NEW
USE IN CONTACT

*-- End OF lpCon_Vend
*:****************************************************************
PROCEDURE lpCon_Sals

SET EXCLUSIVE ON
USE (lcAms_Data+'SALESREP') ALIAS OLD IN 0
USE (gcDataDir+'SALESREP') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'SALESREP') ALIAS NEW IN 0

*
WAIT WINDOW 'Converting SalesRep file.' NOWAIT
SELE OLD
LOCATE
SCAN
  Wait Window 'Converting SalesRep File, Salesrep # : '+ OLD.SALESREP+ ' Please Wait.' NOWAIT
   SELE NEW
   APPE BLANK
   =RLOCK()
   lcTermCode = lfEqv_Code('CTERMCODE ','','T')
   REPLACE REPCODE    WITH OLD.SALESREP,;
           NAME       WITH OLD.REPNAME ,;
           cAddress1  WITH OLD.ADDR1   ,;
           cAddress2  WITH OLD.ADDR2   ,;
           cAddress3  WITH OLD.CITY    ,;
           cAddress4  WITH OLD.STATE   ,;
           cAddress5  WITH OLD.ZIP     ,;
           cAddress6  WITH 'USA'       ,;
           ccont_Code WITH 'USA'       ,;
           Pay_Type   WITH 'R'         ,;
           cTermCode  WITH lcTermCode
           
   lcNum = '0123456789'
   lcPhone=''
   FOR I = 1 TO LEN(ALLTRIM(OLD.PHONE))
     IF SUBSTR(OLD.PHONE,I,1) $ lcNum
       lcPhone = lcPhone + SUBSTR(OLD.PHONE,I,1)
     ENDIF
   ENDFOR
   lcRegion = lfEqv_Code('REGION    ',OLD.MKTREGION,'R')
   REPLACE PHONE   WITH lcPhone  ,;
           REGION  WITH lcRegion ,;
           SSN     WITH OLD.SSN
   lcFax=''
   FOR I = 1 TO LEN(ALLTRIM(OLD.FAX))
     IF SUBSTR(OLD.FAX,I,1) $ lcNum
       lcFax = lcFax + SUBSTR(OLD.FAX,I,1)
     ENDIF
   ENDFOR
   REPLACE FAX     WITH lcFax       ,;
           AGEDATE WITH OLD.AGEDATE ,;
           BALANCE WITH OLD.BALANCE  ,;
           DISCNT1 WITH OLD.DISCORD1,;
           REDCOM1 WITH OLD.REDUCE1 ,;
           DISCNT2 WITH OLD.DISCORD2,;
           REDCOM2 WITH OLD.REDUCE2 ,;
           DISCNT3 WITH OLD.DISCORD3,;
           REDCOM3 WITH OLD.REDUCE3 ,;
           DISCNT4 WITH OLD.DISCORD4,;
           REDCOM4 WITH OLD.REDUCE4 
  
   UNLOCK
   =gfAdd_Info('NEW')
   SELE OLD
ENDSCAN
WAIT CLEAR

USE IN OLD
USE IN NEW

*-- End OF lpCon_Sals
*:****************************************************************
PROCEDURE lpCon_SlCm

SET EXCLUSIVE ON
USE (lcAms_Data+'salescom') ALIAS OLD IN 0
USE (gcDataDir+'repcomm') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'repcomm') ALIAS NEW IN 0

SELECT OLD
SCAN FOR !(STATUS = 'V')
  Wait Window 'Converting SalesRep commission File, Invoice # : '+ OLD.Invoice+ ' Please Wait.' NOWAIT  
  SELECT NEW
  APPEND BLANK
  REPLACE REPCODE     WITH Old.Salesrep  ,;
          Tran        WITH Old.INVOICE   ,;
          Order       WITH Old.Order     ,;
          Account     WITH Old.Account   ,;
          Custpo      WITH Old.Custpo    ,;
          Tran        WITH Old.Salesrep  ,;
          CommPcnt    WITH Old.CommPcnt  ,;
          Amount      WITH Old.CommDue   ,;
          Balance     WITH Old.RunBal    ,;
          Date        WITH Old.InvDate   ,;
          Desc        WITH Old.Desc      ,;
          TranType    WITH Old.TranCd    ,;
          Status      WITH IIF(EMPTY(Old.Status),"O",Old.Status),;
          PayDate     WITH Old.PayDate   ,;
          cCurrCode   WITH gcBaseCurr     ,;
          nCurrunit   WITH 1              ,;
          Nexrate     WITH 1.0000         ,;
          nForamnt    WITH Old.CommDue
  
  =gfAdd_Info('NEW')
  SELECT OLD
ENDSCAN
WAIT CLEAR
USE IN OLD
USE IN NEW
WAIT CLEAR

*- End OF lpCon_SlCm
*:****************************************************************
PROCEDURE lpCon_Ordh

USE (lcAms_Data+'ORDERHDR') ALIAS OLD IN 0
SET EXCLUSIVE ON

USE (gcDataDir+'ORDHDR') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'ORDHDR') ALIAS NEW IN 0
WAIT WINDOW 'Converting Order Header File.' NOWAIT
*
SELE OLD
SCAN FOR !EMPTY(ORDER)  AND ! (Status $ 'CX')
  Wait Window 'Converting Order Header File, Order # : '+ OLD.ORDER + ' Please Wait.' NOWAIT
   SELE NEW
   APPEND BLANK
   =RLOCK()
   lcTermCode = lfEqv_Code('CTERMCODE ',OLD.TERMS,'T')
   lcSpInst   = lfEqv_Code('SPCINST   ',OLD.SPCINST,'I')
   lcShipVia  = lfEqv_Code('SHIPVIA   ',OLD.ShipVia,'T')
  lcSeason = ''
  IF !EMPTY(OLD.SEASON)
    lcSeason = lfUpSeaDiv('SEASON'+OLD.SEASON)
  ENDIF
  lcDivison = ''
  IF !EMPTY(OLD.DIVISION)
    lcDivison = lfUpSeaDiv('Division'+OLD.DIVISION)
  ENDIF
   
   REPLACE cordType   WITH 'O'         ,;
           ORDER      WITH OLD.ORDER   ,;
           STATUS     WITH OLD.STATUS  ,;
           ACCOUNT    WITH OLD.ACCOUNT ,;
           STORE      WITH OLD.STORE   ,;
           DEPT       WITH OLD.DEPT    ,;
           CUSTPO     WITH OLD.CUSTPO  ,;
           NOTE1      WITH OLD.NOTE1   ,;
           NOTE2      WITH OLD.NOTE2   ,;
           PRIORITY   WITH OLD.PRIORITY,;
           SEASON     WITH lcSeason    ,;
           cDIVISION  WITH lcDivison   ,;
           BULK       WITH OLD.DIST    ,;
           MULTI      WITH 'N'         ,;
           cTermCode  WITH lcTermCode  ,;
           SHIPVIA    WITH lcShipVia   ,;
           SPCINST    WITH lcSpInst    ,;
           cINSUR     WITH OLD.INSURCD ,;
           BUYER      WITH OLD.BUYER   ,;
           Link_Code  WITH 'DEFDEF'    ,;
           gl_Sales   WITH 'DEF   '    ,;
           Alt_Shpto  WITH .F.

   lcNum = '0123456789'
   lcPhone=''
   FOR I = 1 TO LEN(ALLTRIM(OLD.PHONE))
     IF SUBSTR(OLD.PHONE,I,1) $ lcNum
       lcPhone = lcPhone + SUBSTR(OLD.PHONE,I,1)
     ENDIF
   ENDFOR
   REPLACE PHONE     WITH lcPhone      ,;
           cFacCode  WITH OLD.FACTOR   ,;
           FACTACCT  WITH OLD.FACTACCT ,;
           APPROVAL  WITH OLD.APPRVLCD ,;
           APPRAMT   WITH OLD.APPROVAL ,;
           REP1      WITH OLD.SALESREP1,;
           COMM1     WITH OLD.COMMPCNT1,; 
           REP2      WITH OLD.SALESREP2,;
           COMM2     WITH OLD.COMMPCNT2,;
           ENTERED   WITH OLD.ENTERED  ,;
           START     WITH OLD.START    ,;
           COMPLETE  WITH OLD.COMPLETE ,;
           DISC      WITH OLD.DISCPCNT ,;
           BOOK      WITH OLD.BOOK     ,;
           BOOKAMT   WITH OLD.BOOKAMT  ,;
           CANCEL    WITH OLD.CANCEL   ,;
           CANCELAMT WITH OLD.CANCELAMT,;
           SHIP      WITH OLD.SHIP     ,;
           SHIPAMT   WITH OLD.SHIPAMT  ,;
           OPEN      WITH OLD.OPEN     ,;
           OPENAMT   WITH OLD.OPENAMT  ,;
           STNAME    WITH OLD.STNAME   ,;
           cAddress1 WITH OLD.STADDR1  ,;
           cAddress2 WITH OLD.STADDR2  ,;
           cAddress3 WITH OLD.STCITY   ,;
           cAddress4 WITH OLD.STSTATE  ,;
           cAddress5 WITH OLD.STZIP    ,;
           CWARECODE WITH lcWare       ,;
           LASTLINE  WITH OLD.nxtline  ,;
           cCurrCode WITH gcBaseCurr   ,;
           nCurrunit WITH 1            ,;
           Nexrate   WITH 1.0000 
           
   =gfAdd_Info('NEW')
   UNLOCK
   SELECT OLD   
ENDSCAN
WAIT CLEAR

USE IN OLD
USE IN NEW
*-- End Of lpCon_Ordh
*:****************************************************************
PROCEDURE lpCon_OrdL


USE (lcAms_Data+'ORDERLN') ALIAS OLD IN 0
SET EXCLUSIVE ON

USE (gcDataDir+'ORDLINE') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE


SET EXCLUSIVE OFF
USE (gcDataDir+'ORDLINE') ALIAS NEW IN 0
USE (gcDataDir+'INVLINE') ORDER Invlineo IN 0

USE (gcDataDir+'ORDHDR') IN 0
SELECT ORDHDR
SET ORDER TO TAG Ordhdr
*
SELE OLD
LOCATE 

WAIT WINDOW 'Converting Order Line file.' NOWAIT
lcOldOrder = ORDER
SCAN FOR !EMPTY(ORDER)
  WAIT WINDOW 'Converting Order Line File, Order# : ' + Old.Order +' Please Wait.' NOWAIT
  *-- Get the Order line From the invoice file.
  IF lcOldOrder # OLD.Order
    = lfGetOrdLn()
    lcOldOrder = OLD.Order
  ENDIF
  
  =SEEK('O'+OLD.Order,'ORDHDR')
  SELECT NEW
  APPEND BLANK
  =RLOCK()
   REPLACE cordType  WITH 'O'            ,;
           ORDER     WITH OLD.ORDER      ,;
           ACCOUNT   WITH OLD.ACCOUNT    ,;
           LINENO    WITH VAL(OLD.LINENO),;
           STORE     WITH OLD.STORE      ,;
           STYLE     WITH PADR(OLD.STYLE,12)+'-'+PADR(OLD.COLOR,6),;
           SCALE     WITH OLD.SCALE      ,;
           GROUP     WITH OLD.PIKGRP     ,;
           PRICE     WITH OLD.PRICE      ,;
           START     WITH ORDHDR.START   ,;
           COMPLETE  WITH ORDHDR.COMPLETE,;
           QTY1      WITH OLD.QTY0       ,;
           QTY2      WITH OLD.QTY1       ,;
           QTY3      WITH OLD.QTY2       ,;
           QTY4      WITH OLD.QTY3       ,;
           QTY5      WITH OLD.QTY4       ,;
           QTY6      WITH OLD.QTY5       ,;
           QTY7      WITH OLD.QTY6       ,;
           QTY8      WITH OLD.QTY7       ,;
           TOTQTY    WITH OLD.QTYTOT     ,;
           PIK1      WITH OLD.PIK0       ,;
           PIK2      WITH OLD.PIK1       ,;
           PIK3      WITH OLD.PIK2       ,;
           PIK4      WITH OLD.PIK3       ,;
           PIK5      WITH OLD.PIK4       ,;
           PIK6      WITH OLD.PIK5       ,;
           PIK7      WITH OLD.PIK6       ,;
           PIK8      WITH OLD.PIK7       ,;
           TOTPIK    WITH OLD.PIKTOT     ,;
           PIKTKT    WITH OLD.PIKTKT     ,;
           PIKDATE   WITH OLD.PIKDATE    ,;
           PICKED    WITH IIF(EMPTY(OLD.PIKDATE),.F.,.T.),;
           INVOICE   WITH OLD.INVOICE   ,;
           INVDATE   WITH OLD.INVDATE   ,;
           PREPAK    WITH OLD.PREPAK    ,;
           PPQTY     WITH OLD.PPQTY     ,;
           COMM1     WITH OLD.COMMPCNT1 ,;
           COMM2     WITH OLD.COMMPCNT2 ,;
           cWareCode WITH lcWare        ,;
           SEASON    WITH Ordhdr.SEASON

REPLACE     Book1        WITH OLD.QTY0  ,;
            Book2        WITH OLD.QTY1  ,;
            Book3        WITH OLD.QTY2  ,;
            Book4        WITH OLD.QTY3  ,;
            Book5        WITH OLD.QTY4  ,;
            Book6        WITH OLD.QTY5  ,;
            Book7        WITH OLD.QTY6  ,;
            Book8        WITH OLD.QTY7  ,;
            TotBook      WITH OLD.QTYTOT
           
   UNLOCK
   =gfAdd_Info('NEW')
   SELECT OLD
ENDSCAN

WAIT CLEAR

USE IN OLD
USE IN NEW
USE IN ORDHDR

*-- End Of lpCon_Ordl
*:****************************************************************
PROCEDURE lpCon_Colr

USE (lcAms_Data+'COLORS') IN 0 

SET EXCLUSIVE ON
USE (gcDataDir+'CODES') IN 0 
SELECT CODES
ZAP
USE
SET EXCLUSIVE OFF
USE (gcDataDir+'CODES') IN 0 
llFirst = .F.

SELECT COLORS
SCAN
  Wait Window 'Converting Color File, Color : '+ COLORS.clrdesc +' Please Wait.' NOWAIT  

  IF !llFirst
    INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No  ,Cdiscrep,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
    VALUES            ('D'     ,'COLOR     ',COLORS.Color ,COLORS.clrdesc,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
    llFirst = .T.
  ENDIF
  
  *- Record No. 1
  INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No  ,Cdiscrep,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
  VALUES            ('N'     ,'COLOR     ',COLORS.Color ,COLORS.clrdesc,.F.       ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
  =gfAdd_Info('CODES')
      
  *- Record No. 2
  INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No  ,lRltFields,cRltField,cRltd_nam       ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
  VALUES            ('N'     ,'COLOR     ',COLORS.Color ,.F.       ,'Y'      ,'CLRLNAME      ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
  =gfAdd_Info('CODES')
  
  *- Record No. 3
  INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No ,lRltFields,cRltField,cRltd_nam       ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
  VALUES            ('N'     ,'COLOR     ',COLORS.Color,.F.       ,'Y'      ,'CNRFCODE      ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
  =gfAdd_Info('CODES')
  
  *- Record No. 4
  INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cRltd_Vlu,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
  VALUES            ('N'     ,'COLOR     ',COLORS.Color,.F.       ,'Y'      ,'LLOBSOLETE','L'      ,'F'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
  =gfAdd_Info('CODES')
  
  *- Record No. 5
  INSERT INTO CODES (cDefCode,cFld_Name ,cCode_No ,lRltFields,cRltField,cRltd_nam           ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
  VALUES            ('N'     ,'COLOR     ',COLORS.Color,.F.       ,'Y'      ,'CPAINT            ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
  =gfAdd_Info('CODES')
  
ENDSCAN
WAIT CLEAR


USE IN COLORS
USE IN CODES



*-- End OF lpCon_Colr
*:****************************************************************
PROCEDURE lpCon_Cods

SET EXCLUSIVE OFF
USE (lcAms_Data+'CODE') ORDER Code IN 0 
USE (gcDataDir+'CODES') IN 0 
USE (gcDataDir+'sequence') ORDER Cseq_type IN 0 
DIMEN laCodes [10,2]
laCodes = ''
laCodes [01,2] = 'TRANCODE'
laCodes [02,2] = 'CCREDITCOD'
laCodes [03,2] = 'CLASS'
laCodes [04,2] = 'REASON'
laCodes [05,2] = 'CSTYGROUP'
laCodes [06,2] = 'SPCINST'
laCodes [07,2] = 'MFGCODE'
laCodes [08,2] = 'REGION'
laCodes [09,2] = 'CTERMCODE'
laCodes [10,2] = 'SHIPVIA'

*-- CCODE_NO - nseq_no
SELECT CODE
LOCATE
SCAN
  Wait Window 'Converting Codes File, Code #: '+ Code+ ' Please Wait.' NOWAIT  
  DO CASE
    *-- Debit Code 'TRANCODE'   
    CASE LEFT(CODE,1) = 'A'
      
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'TRANCODE')
      IF  lnPos > 0 .AND. EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'TRANCODE  ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')
      ENDIF
   
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'TRANCODE  ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

      *- Record No. 2
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'TRANCODE  ',lcnseq_no ,.F.       ,'Y'      ,'ALLOW_TYPE','C'      ,'F',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

      *- Record No. 3
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'TRANCODE  ',lcnseq_no ,.F.       ,'Y'      ,'CADJACCT  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 4
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'TRANCODE  ',lcnseq_no ,.F.       ,'Y'      ,'CBNKCODE  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 5
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'TRANCODE  ',lcnseq_no ,.F.       ,'Y'      ,'CCHKACCT  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
    
    *-- Credit Code CCREDITCOD
    CASE LEFT(CODE,1) = 'B'
      
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'CCREDITCOD')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'CCREDITCOD ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'CCREDITCOD ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 2
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CCREDITCOD ',lcnseq_no ,.F.       ,'Y'      ,'ALLOW_TYPE','C'      ,'F',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 3
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CCREDITCOD ',lcnseq_no ,.F.       ,'Y'      ,'CADJACCT  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

      *- Record No. 4
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CCREDITCOD ',lcnseq_no ,.F.       ,'Y'      ,'CBNKCODE  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 5
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CCREDITCOD ',lcnseq_no ,.F.       ,'Y'      ,'CCHKACCT  ','C'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
    *-- Class Code
    CASE LEFT(CODE,1) = 'C'

      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'CLASS')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'CLASS     ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")        
        =gfAdd_Info('CODES')
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'CLASS     ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")      
      =gfAdd_Info('CODES')
    
    *- Vendor Code [we will not conver this code.]
    CASE LEFT(CODE,1) = 'D'

    *-- Return resson.
    CASE LEFT(CODE,1) = 'E'
    
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'REASON')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'REASON    ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'REASON    ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
    
    *-- Group Code. 
    CASE LEFT(CODE,1) = 'G'

      lnPos = ASCAN(laCodes,'CSTYGROUP ')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'CSTYGROUP ',Substr(Code.code,2,2),Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'CSTYGROUP ',Substr(Code.code,2,2),Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
    
    *-- SpcInst Code.
    CASE LEFT(CODE,1) = 'I'

    
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'SPCINST')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'SPCINST   ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'SPCINST   ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')  
    
    *--  Mf Operation.
    CASE LEFT(CODE,1) = 'M'
    
      lnPos = ASCAN(laCodes,'MFGCODE')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'MFGCODE   ',Substr(Code.code,2,2),Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2),Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 2
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'CCONTCODE ','C'      ,' ',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 3
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'CCONTNAME ','C'      ,' ',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

      *- Record No. 4
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'COPERSEQ  ','C'      ,' ',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 5
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'GLACCOUNT ','C'      ,' ',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
    
      *- Record No. 6
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'LEADTIME  ','N'      ,'0',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 7
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'LINHOUSE  ','L'      ,'T',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 8
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'MFGCODE   ',Substr(Code.code,2,2) ,.F.       ,'Y'      ,'LMFGOPR   ','L'      ,'T',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
    
    *-- Region Code.
    CASE LEFT(CODE,1) = 'R'

      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'REGION')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'REGION    ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'REGION    ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
    *-- Terms code
    CASE LEFT(CODE,1) = 'T'
    
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1

      lnPos = ASCAN(laCodes,'CTERMCODE')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'CTERMCODE ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')        
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 2
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'COD       ','N'      ,'0.00',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 3
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'CCONTNAME ','C'      ,'N',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

      *- Record No. 4
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'EOM       ','C'      ,'Y',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 5
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'LINSTALLM ','L'      ,'F',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
    
      *- Record No. 6
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'NTERDISCD ','N'      ,'0',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 7
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'NTERDISCR ','N'      ,'0.000',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 8
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'CTERMCODE ',lcnseq_no ,.F.       ,'Y'      ,'NTERDUED  ','N'      ,'0',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')

    *-- User Code [we will not conver this code.]
    CASE LEFT(CODE,1) = 'U'

    *-- Shipvia code 
    CASE LEFT(CODE,1) = 'V'
    
      =SEEK('CCODE_NO','sequence')
      lcnseq_no = STR(sequence.nseq_no,6)
      lcnseq_no = STRTRAN(lcnseq_no,' ','0')
      SELECT sequence
      REPLACE nseq_no WITH nseq_no + 1
      
      lnPos = ASCAN(laCodes,'SHIPVIA')
      IF  lnPos > 0 .AND.  EMPTY(laCodes[(lnPos/2),1])
        laCodes[(lnPos/2),1] = 'T'
        *- Record No. 1
        INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
        VALUES            ('D'     ,'SHIPVIA   ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
        =gfAdd_Info('CODES')
      ENDIF
      
      *- Record No. 1
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No ,Cdiscrep  ,lRltFields,cRltField,cAdd_User,dAdd_Date,cAdd_Time  ,CADD_VER);
      VALUES            ('N'     ,'SHIPVIA   ',lcnseq_no,Code.cData,.F.  ,'N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
            
      *- Record No. 2
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,crltd_vlu,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'SHIPVIA   ',lcnseq_no ,.F.       ,'Y'      ,'CUPS      ','C'      ,'F',gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 3
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'SHIPVIA   ',lcnseq_no ,.F.       ,'Y'      ,'NCODCHARGE','N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 4
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'SHIPVIA   ',lcnseq_no ,.F.       ,'Y'      ,'NFXDPRCNT ','N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
      *- Record No. 5
      INSERT INTO CODES (cDefCode,cFld_Name   ,cCode_No  ,lRltFields,cRltField,cRltd_nam   ,crltd_typ,cAdd_User,dAdd_Date,cAdd_Time,CADD_VER);
      VALUES            ('N'     ,'SHIPVIA   ',lcnseq_no ,.F.       ,'Y'      ,'NINSCHARGE','N'      ,gcUser_ID,DATE()   ,gfGetTime(),"A27")
      =gfAdd_Info('CODES')
      
    *-- code  [we will not conver this code.]
    CASE LEFT(CODE,1) = 'W'

     
  ENDCASE
  
  
ENDSCAN

*-- Convert the season and Division.
DO lpCon_Ctrl


WAIT CLEAR
USE IN CODES
USE IN CODE
*-- End OF lpCon_Cods.
*:****************************************************************
PROCEDURE lpCon_Pikt

SET EXCLUSIVE ON
USE (lcAms_Data+'piktkt') ALIAS OLD IN 0
USE (gcDataDir+'piktkt') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'piktkt') ALIAS NEW IN 0

SELECT OLD
SCAN FOR !(PikStat $ 'CXV')
  Wait Window 'Converting Piktkt File, Piktkt # : '+ Old.Piktkt + ' Please Wait.' NOWAIT  
  SELECT NEW
  APPEND BLANK
  REPLACE Piktkt    WITH Old.Piktkt ,;
          Account   WITH Old.Account,;
          Store     WITH Old.Store  ,;
          Order     WITH Old.order  ,;
          Date      WITH Old.Pikdate,;
          cWareCode WITH lcWare     ,;
          Status    WITH IIF(EMPTY(Old.PikStat),'O',Old.PikStat)
  =gfAdd_Info('NEW')
  SELECT OLD
ENDSCAN

USE IN OLD
USE IN NEW
WAIT CLEAR

*- End OF lpCon_Pikt
*:****************************************************************
FUNCTION lfGetOrdLn
PRIVATE lnAlias
lnAlias = SELECT(0)

IF SEEK(lcOldOrder,'INVLINE')
  SELECT INVLINE
  SCAN REST WHILE order+STR(lineno,6)+invoice = lcOldOrder
    SELECT NEW
    APPEN BLANK
    REPLACE cOrdType    WITH 'O'            ,;
            Order       WITH INVLINE.Order  ,;
            Account     WITH INVLINE.Account,;
            cWareCode   WITH lcWare         ,;
            LineNo      WITH InvLine.LineNo ,;
            Store       WITH InvLine.Store  ,;
            Style       WITH InvLine.Style  ,;
            SEASON      WITH InvLine.Season ,;
            Scale       WITH Invline.Scale  ,;
            PrePak      WITH Invline.PrePak ,;
            Price       WITH Invline.Price  ,;
            START       WITH {},;
            Complete    WITH {},;
            QTY1        WITH 0 ,;
            QTY2        WITH 0 ,;
            QTY3        WITH 0 ,;
            QTY4        WITH 0 ,;
            QTY5        WITH 0 ,;
            QTY6        WITH 0 ,;
            QTY7        WITH 0 ,;
            QTY8        WITH 0 ,;
            TotQty      WITH 0 ,;
            Book1       WITH Invline.Qty1 ,;
            Book2       WITH Invline.Qty2 ,;
            Book3       WITH Invline.Qty3 ,;
            Book4       WITH Invline.Qty4 ,;
            Book5       WITH Invline.Qty5 ,;
            Book6       WITH Invline.Qty6 ,;
            Book7       WITH Invline.Qty7 ,;
            Book8       WITH Invline.Qty8 ,;
            TotBook     WITH Invline.TotQty,;
            Picked      WITH .F.

    =gfAdd_Info('NEW')
    SELECT INVLINE
    
  ENDSCAN
  
ENDIF

SELECT(lnAlias)
*-- End OF lfGetOrdLn
*:****************************************************************
PROCEDURE lpCon_Ctrl
Private lnPrv_als

lnPrv_als = SELECT(0)

USE (lcAms_Dir+'control') IN 0 
SELECT CONTROL
LOCATE FOR ALLTRIM(dD) = 'DATA1'

*-- Convert the Season.
SELECT CODES
IF !EMPTY(control.Seasona)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasona,6),;
          cDisCrep    WITH control.Seasona        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.


  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'D'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasona,6),;
          cDisCrep    WITH control.Seasona        ,;
          cRltfield   WITH 'N'

  =gfAdd_Info('CODES')        
ENDIF

*-- 2
IF !EMPTY(control.Seasonb)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasonb,6),;
          cDisCrep    WITH control.Seasonb        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.
          
  =gfAdd_Info('CODES')
ENDIF


*-- 3
IF !EMPTY(control.Seasonc)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasonc,6),;
          cDisCrep    WITH control.Seasonc        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.

          
  =gfAdd_Info('CODES')
ENDIF


*-- 4
IF !EMPTY(control.Seasond)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasond,6),;
          cDisCrep    WITH control.Seasond        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.
          
  =gfAdd_Info('CODES')
ENDIF

*-- 5
IF !EMPTY(control.Seasone)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasone,6),;
          cDisCrep    WITH control.Seasone        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.
          
  =gfAdd_Info('CODES')
ENDIF

*-- 6
IF !EMPTY(control.Seasonf)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasonf,6),;
          cDisCrep    WITH control.Seasonf        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.
          
  =gfAdd_Info('CODES')
ENDIF

*-- 7
IF !EMPTY(control.Seasony)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'SEASON    '           ,;
          cCode_No    WITH LEFT(control.Seasony,6),;
          cDisCrep    WITH control.Seasony        ,;
          cRltfield   WITH 'N'                    ,;
          lrltfields  WITH .F.
          
  =gfAdd_Info('CODES')
ENDIF


*-- Convert the Division.
*-- 1
IF !EMPTY(control.Divisiona)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiona,6),;
          cDisCrep    WITH control.DivNamea,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'D'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiona,6),;
          cDisCrep    WITH control.DivNamea,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiona,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

*-- 2
IF  !EMPTY(control.DivisionB)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.DivisionB,6),;
          cDisCrep    WITH control.DivisionB,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.DivisionB,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

*-- 3
IF  !EMPTY(control.Divisionc)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisionc,6),;
          cDisCrep    WITH control.Divisionc,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisionc,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

*-- 4 
IF  !EMPTY(control.Divisiond)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiond,6),;
          cDisCrep    WITH control.Divisiond,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')

  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiond,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
  
ENDIF

*-- 5
IF  !EMPTY(control.Divisione)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisione,6),;
          cDisCrep    WITH control.Divisione,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisione,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

*-- 6
IF  !EMPTY(control.Divisionf)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.DivisionF,6),;
          cDisCrep    WITH control.Divisionf,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisionf,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

*-- 7
IF  !EMPTY(control.Divisiong)
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiong,6),;
          cDisCrep    WITH control.Divisiong,;
          cRltfield   WITH 'N'
  =gfAdd_Info('CODES')
  
  APPEN BLANK
  REPLACE cdefCode    WITH 'N'                    ,;
          cfld_name   WITH 'CDIVISION '           ,;
          cCode_No    WITH LEFT(control.Divisiong,6),;
          cRltfield   WITH 'Y'                    ,;
          crltd_nam   WITH 'DIVLNAME  '           ,;
          crltd_typ   WITH 'C'
  =gfAdd_Info('CODES')
ENDIF

USE IN control
SELECT(lnPrv_als)

*- end of lpCon_Ctrl
*:****************************************************************
PROCEDURE lpCon_Rah

*-- rahdr

USE (lcAms_Data+'rahdr') ALIAS OLD IN 0
SET EXCLUSIVE ON

USE (gcDataDir+'retauth') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'retauth') ALIAS NEW IN 0
WAIT WINDOW 'Converting Return Header file.' NOWAIT
SELECT OLD
SCAN
  SELECT NEW
  WAIT WINDOW 'Converting Return Header File, RA #' + Old.RANO + ' Please Wait.' NOWAIT
  APPEND BLANK
  lcReason = lfEqv_Code('REASON    ',Old.REASON,'E')
  
  lcDivison = ''
  IF !EMPTY(OLD.DIVISION)
    lcDivison = lfUpSeaDiv('Division'+OLD.DIVISION)
  ENDIF
  
  REPLACE RANO        WITH Old.RANO    ,;
          Account     WITH Old.Account ,;
          Store       WITH Old.Store   ,;
          Status      WITH Old.Status  ,;
          raDate      WITH Old.InvDate ,;
          Void        WITH Old.Void    ,;
          REASON      WITH lcReason    ,;
          Invoice     WITH Old.Invoice ,;
          cDivision   WITH lcDivison   ,;
          CIntr_inv   WITH 'N'         ,;
          Order       WITH Old.Order   ,;
          Custpo      WITH Old.Custpo  ,;
          Auth        WITH Old.Auth    ,;
          Authamt     WITH Old.Authamt ,;
          RetDate     WITH Old.Entered ,;
          Return      WITH Old.Return  ,;
          ReturnAmt   WITH Old.ReturnAmt,;
          Tran        WITH ''          ,;
          cretNote1   WITH Old.Note1   ,;
          cretNote2   WITH Old.Note2   ,;
          Cartons     WITH Old.Cartons ,;
          CwareCode   WITH lcWare      ,;
          nRtOpnamt   WITH 0           ,;
          nreta_dam   WITH 0           ,;
          nReta_can   WITH 0           ,;
          cCurrCode  WITH gcBaseCurr   ,;
          nCurrunit  WITH 1            ,;
          Nexrate    WITH 1.0000       ,;
          lHasNotes  WITH .F.
  
  =gfAdd_Info('NEW')
    
  SELECT OLD
ENDSCAN

USE IN OLD
USE IN NEW

*-- End OF lpCon_Rah
*:****************************************************************
PROCEDURE lpCon_Ral

*-- raline
USE (lcAms_Data+'rahdr') ORDER Rano IN 0

USE (lcAms_Data+'raline') ALIAS OLD IN 0
SET EXCLUSIVE ON

USE (gcDataDir+'raline') ALIAS NEW IN 0
SELECT NEW
ZAP
*
USE

SET EXCLUSIVE OFF 
USE (gcDataDir+'raline') ALIAS NEW IN 0
WAIT WINDOW 'Converting Return Line File.' NOWAIT
SELECT OLD
SCAN
  WAIT WINDOW 'Converting Return Line File, RA #' + Old.RANO + ' Please Wait.' NOWAIT
  =SEEK(RANO,'rahdr')
  lcReason = lfEqv_Code('REASON    ',Rahdr.Reason,'E')
  SELECT NEW
  APPEND BLANK


  REPLACE RANO       WITH Old.RANO    ,;
          Account    WITH Old.Account ,;
          STYLE      WITH PADR(Old.STYLE,12)+'-'+ PADR(Old.COLOR,6),;
          cra_linno  WITH PADR(ALLTRIM(Old.LinenO),4),;
          Reason     WITH lcReason    ,;
          Price      WITH Old.Price   ,;
          Qty1       WITH Old.Ret0    ,;
          Qty2       WITH Old.Ret1    ,;
          Qty3       WITH Old.Ret2    ,;
          Qty4       WITH Old.Ret3    ,;
          Qty5       WITH Old.Ret4    ,;
          Qty6       WITH Old.Ret5    ,;
          Qty7       WITH Old.Ret6    ,;
          Qty8       WITH Old.Ret7    ,;
          TotQty     WITH Old.RetTot  ,;
          Amount     WITH (Old.Price * Old.RetTot),;
          Tax_Rate   WITH 0.0         ,;
          nPstrate   WITH 0.0         ,;
          nOpnQty1   WITH 0           ,;
          nOpnQty2   WITH 0           ,;
          nOpnQty3   WITH 0           ,;
          nOpnQty4   WITH 0           ,;
          nOpnQty5   WITH 0           ,;
          nOpnQty6   WITH 0           ,;
          nOpnQty7   WITH 0           ,;
          nOpnQty8   WITH 0           ,;
          nTotOpnQty WITH 0 

  =gfAdd_Info('NEW')
  SELECT OLD
ENDSCAN

USE IN OLD
USE IN NEW
*-- End OF lpCon_Ral

PROCEDURE lpCon_StyJ
lcRISessn = gfSequence('GLSESSION')
SELECT 0
SET EXCLUSIVE ON
USE (gcDataDir+'StyInvJl')
ZAP
SET EXCLUSIVE OFF
SELECT 0
USE (gcDataDir+'Style')
SET ORDER TO 1
SCAN
  WAIT WINDOW STYLE NOWAIT
  IF TotStk <> 0
    SELECT StyInvJl
    APPEND BLANK
    REPLACE csession  with lcRISessn   ,;
            Style     with style.style ,;
            cWareCode WITH lcWare      ,;
            dTrDate   WITH DATE()      ,;
            cTrType   WITH '2'         ,;
            cTrCode   WITH lcRISessn   ,;
            nCost     WITH 0           ,;
            cIRType   WITH IIF(STYLE.TOTSTK>0,'R','I'),;
            cRSession WITH lcRISessn   ,;
            nStk1     WITH Style.Stk1  ,;
            nStk2     WITH Style.Stk2  ,;
            nStk3     WITH Style.Stk3  ,;                        
            nStk4     WITH Style.Stk4  ,;
            nStk5     WITH Style.Stk5  ,;
            nStk6     WITH Style.Stk6  ,;
            nStk7     WITH Style.Stk7  ,;
            nStk8     WITH Style.Stk8  ,;                                                            
            nTotstk   WITH Style.TotStk,;
            nStkVal   WITH 0           ,;
            nPrvsQty  WITH 0           ,;
            nPrvsVal  WITH 0           ,;
            Reference WITH 'Beginning Balance',;
            cAdd_User WITH 'Aria'
            
            
   ENDIF       
ENDSCAN
WAIT CLEAR

*-- End OF lpCon_StyJ

PROCEDURE lpCon_Seq
IF !USED(gcDataDir+'Sequence')
  USE (gcDataDir+'sequence') ORDER Cseq_type IN 0 
ENDIF
USE (lcAms_Data+'seqctl')  IN 0
SELECT Sequence
SCAN
  IF cSeq_Type = 'ORDER'
    REPLACE nSeq_No WITH seqctl.Order
  ENDIF
  IF cSeq_Type = 'PO        '
    REPLACE nSeq_No WITH seqctl.PoNo
  ENDIF
  IF cSeq_Type = 'PIKTKT    '
    REPLACE nSeq_No WITH seqctl.Ticket
  ENDIF
  IF cSeq_Type = 'INVOICE   '
    REPLACE nSeq_No WITH seqctl.Invoice
  ENDIF
  IF cSeq_Type = 'RANO      '
    REPLACE nSeq_No WITH seqctl.RaNo
  ENDIF
ENDSCAN
USE IN Sequence
*-- End OF lpCon_Seq

*:****************************************************************
Function lfEqv_Code
PARAMETER lcSnd_FNme , lcOld_Code , lcSndParam
PRIVATE lnAlias , lcEq_Code
lcEq_Code = ''

lnAlias = SELECT(0)
USE (lcAms_Data+'CODE') ORDER Code    IN 0 
USE (gcDataDir+'CODES') ORDER Ccode_no IN 0 

IF EMPTY(lcOld_Code)
  SELECT CODES
  =SEEK('D'+lcSnd_FNme)
  lcEq_Code = cCode_No
ELSE

  IF SEEK(lcSndParam+lcOld_Code,'CODE')
    lcFld_Desc = Code.cData
    SELECT CODES
    IF SEEK('N'+lcSnd_FNme)
      LOCATE REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+lcSnd_FNme ;
      FOR ALLTRIM(cDiscrep) = ALLTRIM(lcFld_Desc)
      IF FOUND()
        lcEq_Code = Codes.cCode_No
      ELSE
        =SEEK('D'+lcSnd_FNme)
        lcEq_Code = cCode_No
      ENDIF
    ENDIF
  ENDIF
ENDIF

SELECT (lnAlias)

USE IN CODE
USE IN CODES

RETURN lcEq_Code
*-- End Of lfEqv_Code.
*:****************************************************************
FUNCTION lfUpSeaDiv
PARAMETER lcSend_Cod
PRIVATE lnPrvAlias

lnPrvAlias = SELECT(0)

USE (lcAms_Dir+'control') IN 0 
SELECT CONTROL
LOCATE FOR ALLTRIM(dD) = 'DATA1'


lcRetFld = &lcSend_Cod

USE IN control

SELECT (lnPrvAlias)
RETURN lcRetFld


*-- End OF lfUpSeaDiv
*:****************************************************************