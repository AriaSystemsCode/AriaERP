*****************************************************************************
*: Program file  : ConvWind.PRG (C#101718)
*: Program desc. : Custom Convert files .
*: Module        : None.
*:         System: None. (Stand Alone)
*:      Developer: Ashraf Shereif - (ASH)
*****************************************************************************
*: Calls :
*:        PROCEDURES : None
*:        FUNCTIONS  : lfAddMInfo ,lfGetAcc,lfGetSeq.
*:----------------------------------------------------------------------------------
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*:Commentes :
*:           * This program run as stand alone Prg.
*****************************************************************************
*--- Start Main Program
SELECT 1
USE ARCUSTO
SELECT 2
USE ARSHIPTO EXCL
INDEX ON CUSTNO TAG ARSHIPTO OF ARSHIPTO
SELECT 3
USE CUSTOMER
SET ORDER TO 1
SELECT 4
USE ARINVOI
SELECT 5
USE DEBIT
SELECT 6
USE CREDIT
SELECT 7
USE SEQUENCE
SET ORDER TO 1

** Customer file.
SELECT ARCUSTO
SCAN 
  lcAccount = ''
  WAIT WINDOW 'Account : ' + ALLTRIM(Company) NOWAIT
  IF LEN(ALLTRIM(Company)) = 1
    SELECT CUSTOMER
    APPEND BLANK
    REPLACE TYPE    WITH 'M' ,;
            Account WITH ARCUSTO.CustNo
    LOOP
  ENDIF
  =lfGetAcc()  
  SELECT Customer
  APPEND BLANK
  =lfAddMInfo()          
  REPLACE TYPE       WITH 'M'             ,;
          STNAME     WITH ARCUSTO.COMPANY ,;
          PHONE1     WITH ARCUSTO.PHONE   ,;
          BUYER      WITH ARCUSTO.CONTACT ,;
          KEEPER     WITH ARCUSTO.CONTACT2,;
          PHONE2     WITH ARCUSTO.PHONE2  ,;
          NOTE       WITH ARCUSTO.Title   ,;
          Fax        WITH ARCUSTO.Phone3  ,;
          nTaxRate   WITH ARCUSTO.Tax     ,;
          cAddress12 WITH ARCUSTO.Address1,;
          cAddress22 WITH ARCUSTO.Address2,;
          cAddress32 WITH ARCUSTO.City    ,;
          cAddress42 WITH ARCUSTO.State   ,;
          cAddress52 WITH ARCUSTO.Zip
          
  IF ARCUSTO.MULTSHIP AND SEEK(ARCUSTO.CustNo,'ARSHIPTO')
    SELECT ARSHIPTO
    SCAN WHILE CustNo = ARCUSTO.CustNo
      SELECT Customer
      APPEND BLANK
      =lfAddMInfo()
      REPLACE TYPE       WITH 'S'              ,;
              STORE      WITH ARSHIPTO.SHIPNO  ,;
              STNAME     WITH ARSHIPTO.COMPANY ,;
              PHONE1     WITH ARSHIPTO.PHONE   ,;
              BUYER      WITH ARSHIPTO.CONTACT ,;
              cAddress12 WITH ARSHIPTO.Address1,;
              cAddress22 WITH ARSHIPTO.Address2,;
              cAddress32 WITH ARSHIPTO.City    ,;
              cAddress42 WITH ARSHIPTO.State   ,;
              cAddress52 WITH ARSHIPTO.Zip
    ENDSCAN
  ENDIF

ENDSCAN
WAIT CLEAR

** Debit && Credit files.
SELECT ARINVOI
SCAN FOR !EMPTY(CustNo) AND ITOTAL <> AmtPaid
  WAIT WINDOW 'Account : ' + ALLTRIM(Company) NOWAIT
  lcAccount = ''
  =lfGetAcc()
  IF !SEEK('M'+lcAccount,'CUSTOMER')
    SELECT CUSTOMER
    APPEND BLANK
    REPLACE TYPE    WITH 'M' ,;
            Account WITH lcAccount
  ENDIF
  SELECT ARINVOI
  DO CASE
    CASE ITOTAL > AmtPaid
      lnSeqNo=lfGetSeq('DEBIT')
      SELECT Debit
      APPEND BLANK
      REPLACE TranType  WITH '2' ,;
              DueDate   WITH ARINVOI.DueDate 
      REPLACE Account   WITH lcAccount       ,;
              Tran      WITH lnSeqNo         ,;
              TranDate  WITH ARINVOI.InvDate ,;
              dPostDate WITH ARINVOI.InvDate ,;
              Reference WITH ARINVOI.InvNo   ,;
              Amount    WITH ARINVOI.ITOTAL-ARINVOI.AmtPaid  ,;
              cCurrCode WITH 'USD'           ,;
              nCurrUnit WITH 1               ,;
              nExRate   WITH 1
    CASE ITOTAL < AmtPaid
      lnSeqNo=lfGetSeq('CREDIT')
      SELECT Credit
      APPEND BLANK
      REPLACE TranType WITH '5' 
      REPLACE Account   WITH lcAccount       ,;
              Tran      WITH lnSeqNo         ,;
              TranDate  WITH ARINVOI.InvDate ,;
              dPostDate WITH ARINVOI.InvDate ,;
              Reference WITH ARINVOI.InvNo   ,;
              Amount    WITH ARINVOI.ITOTAL-ARINVOI.AmtPaid  ,;
              cCurrCode WITH 'USD'           ,;
              nCurrUnit WITH 1               ,;
              nExRate   WITH 1
  ENDCASE
ENDSCAN
WAIT CLEAR

*****************************************************************************
*: Program file  : lfAddMInfo (C#101718)
*: Program desc. : Function to add information .
*: Module        : None.
*:         System: Aria Apparel System (A2.7)
*:      Developer: Ashraf Shereif - (ASH)
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
FUNCTION lfAddMInfo

REPLACE BILLTO     WITH 'M'             ,;
        Account    WITH lcAccount       ,;
        STATUS     WITH 'A'             ,;
        CONSOL     WITH 'N'             ,;
        BTNAME     WITH ARCUSTO.COMPANY ,;
        Disc       WITH ARCUSTO.Disc    ,;
        CrLimit    WITH ARCUSTO.Limit   ,;
        CrAvail    WITH ARCUSTO.Balance ,;
        Priority   WITH '5'             ,;
        PriceLvl   WITH 'A'             ,;
        cInsur     WITH 'Y'             ,;
        Prnt_Statm WITH 'Y'             ,;
        cAddress1  WITH ARCUSTO.Address1,;
        cAddress2  WITH ARCUSTO.Address2,;
        cAddress3  WITH ARCUSTO.City    ,;
        cAddress4  WITH ARCUSTO.State   ,;
        cAddress5  WITH ARCUSTO.Zip     
        

*****************************************************************************
*: Program file  : lfGetAcc (C#101718)
*: Program desc. : fUNCTION TO GET THE ACCOUNT CODE .
*: Module        : None.
*:         System: Aria Apparel System (A2.7)
*:      Developer: Ashraf Shereif - (ASH)
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
FUNCTION lfGetAcc

lcAcName = ALLTRIM(STRTRAN(COMPANY,'THE '))
lcAcName = ALLTRIM(STRTRAN(lcAcName,'&',' '))
  
IF ' ' $ (lcAcName)    && more than one word.
  lcPreAcc  = SUBSTR(lcAcName,1,1)
  lnSpPos   = ATC(' ',lcAcName)
  lcAcName = ALLTRIM(SUBSTR(lcAcName,lnSpPos,30-lnSpPos))
  lcPreAcc  = lcPreAcc + SUBSTR(lcAcName,1,1)
ELSE && Only one word.
  lcPreAcc = SUBSTR(lcAcName,1,2)    
ENDIF
IF SEEK('M'+lcPreAcc,'Customer')
  SELECT Customer
  SCAN WHILE TYPE+ACCOUNT = 'M'+lcPreAcc
    lcSeq = SUBSTR(ACCOUNT,3,3)
  ENDSCAN
  lnSeq = INT(VAL(lcSeq))
  lnSeq = lnSeq + 1
  DO CASE 
    CASE lnSeq < 10
      lcSeq = '00' + ALLTRIM(STR(lnSeq))
    CASE lnSeq < 100
      lcSeq = '0' + ALLTRIM(STR(lnSeq))
    OTHERWISE
      lcSeq = ALLTRIM(STR(lnSeq))
  ENDCASE
ELSE
  lcSeq = '001'
ENDIF
lcAccount = lcPreAcc + lcSeq

*****************************************************************************
*: Program file  : lfGetSeq (C#101718)
*: Program desc. : fUNCTION TO GET TRANS. SEQ .
*: Module        : None.
*:         System: Aria Apparel System (A2.7)
*:      Developer: Ashraf Shereif - (ASH)
*****************************************************************************
*: Calls : None.
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
FUNCTION lfGetSeq
PARAMETER lcDbCr

SELECT Sequence
IF SEEK(lcDbCr)
  REPLACE nSeq_No WITH nSeq_No + 1
ELSE
  APPEND BLANK
  REPLACE cSeq_Type WITH lcDbCr  ,;
          nSeq_No   WITH 2       ,;
          nFld_Wdth WITH 6       ,;
          cData_Typ WITH 'C'     ,;
          cFile_Nam WITH 'CREDIT',;
          cFile_Tag WITH 'CRTRAN'
ENDIF
lcSeqNo = ALLTRIM(STR(nSeq_No - 1))
lnZeros = REPLICATE('0',6-LEN(lcSeqNo))
lcSeqNo =  lnZeros + lcSeqNo 
RETURN(lcSeqNo)