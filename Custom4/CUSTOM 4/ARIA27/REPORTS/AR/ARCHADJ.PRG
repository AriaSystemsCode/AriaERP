*:************************************************************************
*: Program file  : ARCHADJ.Prg
*: Program desc. : CASH & ADJUSTMENTS JOURNAL REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AR
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 08/30/1998
*: REF           : *300985,1
*: Notes         : This program is built on the standard program AR930
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfvCAB(), lfvCashTyp(), lfsrvSty()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO ARCHADJ
*:************************************************************************
*B602666,1 AMM 03/31/1999 Remove STYLE GROUP from the option grid and add Debit
*B602666,1 AMM            and credit adjustment reasons and payment type.
*B802253,1 BWA 05/27/1999 Print date period in the report header
*B603079,1 MAB 07/04/1999 Add Multi currency format to the report.
*B802767,1 ABD 11/18/1999 Hide the reason CODE from the report& Print Reason Desc.
*E301356,1 ABD 30/11/1999 Add a new option of sorting in case of adjustment only.
*B603410,1 NAD 01/31/2000 Fix bug of Adjust column width in Amount column 
*E301429,1 NAD 06/18/2000  1)Increase the amount field.
*E301429,1                 2)Add record to syrepuvr to ask the user to print
*E301429,1                 decimals or not.  
*B803545,1 BWA 08/20/2000 Fix the bug of if you select only to print the ADJ. 
*B803545,1                and select a specific credit ADJ reason , you will 
*B803545,1                get all the other reason in the report.
*B604144,1 KAM 22/01/2001 Increase picture of Total Amount from 11,2 to 13,2.
*B604274,1 AMM 03/01/2001 Replace the word "STORE" with "Check" in the title of the report
*B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
*E605817,1 SSE 04/21/2002 Enhancements adding Customer Sales Rep In Range to option grid.
*:************************************************************************
*  TRAN CODES             FILE-ID
*  0 = CREDIT MEMOS       CREDIT
*  1 = INVOICES           DEBIT
*  3 = CHARGEBACK IN      DEBIT
*  4 = PAYMENT            CREDIT
*  5 = CREDIT ADJUSTMENT  CREDIT
*  2 = DEBIT ADJUSTMENT   DEBIT
*  6 = CREDIT ON ACCOUNT  CREDIT
*  7 = ALLOWANCE          CREDIT
*  8 = CHR.BCK            ARHIST
*  9 = CREDIT ON ACCT     ARHIST

WORKFILE = gfTEMPNAME()
TRANFILE = gfTEMPNAME()
DIMENSION TAMT(3)
TAMT     = 0.00
ROW      = 99
PAGENO   = 0
XTIME    = TIME()

*E605817,1 Modify Customer.Rep1 in lcRpExp. [Begin]
PRIVATE lcSlRepExp , lcNewSRExp
STORE ".T." TO lcSlRepExp , lcNewSRExp
*-- we need to convert Customer.Rep1 in filter Expr. if found to be (Customer.Rep1 OR Customer.Rep2)
IF "CUSTOMER.SALESREP" $ lcRpExp            
  DIMENSION laTempExpr[1]

  *-- Copy all laOGFxFlt to another array    
  =ACOPY(laOGFxFlt,laTempExpr)

  DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]

  *-- Copy Browse Array to another Temp array
  =ACOPY(laBrFldFlt,laBrTmpFlt)

  DIMENSION laOGFxFlt[1,7]
  PRIVATE lcBrowPos
    
  *-- Change the position of both Design Code & Color in the Second
  *-- Dimensional array Column no 5 --> laOGFxFlr:4 , laOGFxFlr:5
  DIMENSION laBrFldFlt[1,5]
      
  *-- Convert numeric position to string
  lcBrowPos = ALLTRIM(STR(lnSlRepPos))

  FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
    IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
      =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
    ENDIF
  ENDFOR
  laBrFldFlt[1,5] = "laOGFxFlt:1"

  =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnSlRepPos,1),7)
          
  *-- Generate InList expression for SalesRep
  lcSlRepExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  lcNewSRExp = "(" + lcSlRepExp + [ .OR. ] + STRTRAN(lcSlRepExp,".SALESREP",".REP2") + ")"
            
  *lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,lcNewSRExp)
      
  *-- Restore Browse Array from the Temp array
  DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]      
  =ACOPY(laBrTmpFlt,laBrFldFlt)

  DIMENSION laOGFxFlt[1]
  =ACOPY(laTempExpr,laOGFxFlt)    && restore Original array
ENDIF
*E605817,1 Modify Customer.Rep1 in lcRpExp. [End]

*B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
*DASHLN    = REPLICATE('-',132)
*DOTLN     = REPLICATE('.',132)
*DASHLN2   = REPLICATE('=',132)

DASHLN    = REPLICATE('-',130)
DOTLN     = REPLICATE('.',130)
DASHLN2   = REPLICATE('=',130)
*B604303,1 [END]

*B802767,1  Define New Variable To Hold The Desc Reason .[ Begin ]
STORE '' To lcTranCods 
*B802767,1 [ End ]

R_TITLE = 'CASH & ADJUSTMENTS JOURNAL'
QTOPIC='CASH AND ADJUSTMENTS JOURNAL'

*B602666,1 AMM start
*B602666,1 AMM make the Debit adj reason effective only in debit adjustment 
*B602666,1 AMM transactions.
DIMENSION laTOGVR[1]
=ACOPY(laOGVrFlt,laTOGVR)
lnDrAdjP = ASCAN(laTOGVR,"IIF(TRANTYPE='2',DEBIT.TRANCODE,'')")
IF lnDrAdjP # 0
  lnDrAdjP = ASUBSCRIPT(laTOGVR,lnDrAdjP,1)
  laTOGVR[lnDrAdjP,1] = "IIF(TRANTYPE='2',ARHIST.TRANCODE,laTOGVR[lnDrAdjP,6])"
ENDIF
*B602666,1 AMM make the Credit adj reason effective only in credit adjustment
*B602666,1 AMM transactions.
lnCrAdjP = ASCAN(laTOGVR,"IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')")
IF lnCrAdjP # 0
  lnCrAdjP = ASUBSCRIPT(laTOGVR,lnCrAdjP,1)
  
  *B803545,1 Add the type 7 to the filter to get the selected credit adjusments.[START]
  *laTOGVR[lnCrAdjP,1] = "IIF(TRANTYPE='5',ARHIST.TRANCODE,laTOGVR[lnCrAdjP,6])"
  laTOGVR[lnCrAdjP,1] = "IIF(TRANTYPE $'57',ARHIST.TRANCODE,laTOGVR[lnCrAdjP,6])"
  *B803545,1 [END]
    
ENDIF
*B602666,1 AMM make the Payment type filter effective only in payment
*B602666,1 AMM transactions.
lnPyFltP = ASCAN(laTOGVR,"IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')")
IF lnPyFltP # 0
  lnPyFltP = ASUBSCRIPT(laTOGVR,lnPyFltP,1)
  laTOGVR[lnPyFltP,1] = "IIF(TRANTYPE='4',ARHIST.CARPTYPE, laTOGVR[lnPyFltP,6])"
ENDIF
*B602666,1 AMM Form the lcRpExp variable.
lcTOGVr=gfGenFlt('laTOGVR' , .T. , .T.)
lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFxFlt) AND !EMPTY(lcTOGVR),lcOGFxFlt+'|Ÿ',lcOGFxFlt)+;
           lcTOGVr+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFxFlt)AND !EMPTY(lcTOGVR),'Ÿ','')
IF EMPTY(lcOGFxFlt) AND EMPTY(lcTOGVr)
  lcRpExp=lcOGHDFlt
ELSE
  lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
          ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
ENDIF          
lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)        
*B602666,1 AMM end

*-- Remove the word 'Credit.' from the filter expression (THIS FILTER 
*-- WILL BE USED ON debit AND arhist ALSO)
lcRpExp = STRTRAN(UPPER(lcRpExp),'CREDIT.TRANDATE','TRANDATE')
lcRpExp = STRTRAN(UPPER(lcRpExp),'CREDIT.BATCH','BATCH')
*B602666,1 AMM Comment out, don't add to the main filter, form a separate 
*B602666,1 AMM variable instead and add it to the filter expression.
*lcRpExp = '(' + lcRpExp + ')'+ " .AND. AMOUNT<>0"
*-- Add the filter of cash & Adj type
*DO CASE
  *CASE lcrpCAB  = 'C'                                 &&-CASH ONLY
    *lcRpExp = '('+ lcRpExp + ')'+ ".AND.TRANTYPE $'4'"
  *CASE lcrpCAB  = 'A'                                 &&-ADJUSTMENTS ONLY
    *lcRpExp = '('+ lcRpExp + ')'+ ".AND.TRANTYPE $'257'"
  *CASE lcrpCAB  = 'B'
    *lcRpExp = '('+ lcRpExp + ')'+ ".AND.TRANTYPE $'2457'"
*ENDCASE
*-- Add the filter of Factored & Nofactored accounts
*DO CASE
  *CASE lcrpFAcct='F'
    *lcRpExp = '('+ lcRpExp + ')'+ " .AND. !EMPTY(Customer.cFacCode) "
  *CASE lcrpFAcct='N'
  *lcRpExp = '('+ lcRpExp + ')'+ " .AND. EMPTY(Customer.cFacCode) "
*ENDCASE

lcRpFlt = " .AND. AMOUNT<>0 "
*-- Add the filter of cash & Adj type
DO CASE
  CASE lcrpCAB  = 'C'                                 &&-CASH ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'4' "
  CASE lcrpCAB  = 'A'                                 &&-ADJUSTMENTS ONLY
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'257' "
  CASE lcrpCAB  = 'B'
    lcRpFlt = lcRpFlt +" .AND.TRANTYPE $'2457'"
ENDCASE
*-- Add the filter of Factored & Nofactored accounts
DO CASE
  CASE lcrpFAcct='F'
    lcRpFlt = lcRpFlt + " .AND. !EMPTY(Customer.cFacCode) "
  CASE lcrpFAcct='N'
    lcRpFlt = lcRpFlt + " .AND. EMPTY(Customer.cFacCode) "
ENDCASE
*B602666,1 AMM end

*B603079,1 Define Currency Filter [Begin]
lcCurrFilt = ""
*-- if Print in Forign currency (Filter on selected currency)
IF llRpForCur
  lcCurrFilt = [cCurrCode = lcRpCurr]
ENDIF 
*B603079,1 Define Currency Filter [End  ]

*--------------------------------------------
* SELECT RECORDS FROM ARHIST FILE
*--------------------------------------------
SELECT ARHIST
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
*B602666,1 AMM Form the filter expression
*XFILTER = lcRpExp

*E605817,1 Modify lcRpExp. [Begin]
lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,lcNewSRExp)
*E605817,1 Modify lcRpExp. [End]

XFILTER = lcRpExp + lcRpFlt
*B603079,1 Add Currency Filter [Begin]
IF !EMPTY(lcCurrFilt)
  =lfAddCurr()
ENDIF
*B603079,1 Add Currency Filter [End  ]

*B602666,1 AMM end
XFILTER = XFILTER + " .AND. !(TRANTYPE $ '89') "
WAIT WINDOW 'SELECTING A/R HISTORY RECORDS ...' NOWAIT
SET TALK ON
COPY ALL TO (gcWOrkDir+TRANFILE) FOR &XFILTER
SET TALK OFF

SELECT ARHIST
SET RELATION TO
=gfOpenFile(gcWorkDir+TRANFILE,'','EX')

*---------------------------------------
* SELECT RECORDS FROM OPEN CREDIT FILE
*---------------------------------------
WAIT WINDOW 'SELECTING CREDIT RECORDS ...' NOWAIT

SET TALK ON
*B602666,1 AMM Form the filter expression, Reform the lcRpExp variable.
*XFILTER = lcRpExp
DIMENSION laTOGVR[1]
=ACOPY(laOGVrFlt,laTOGVR)
*B602666,1 AMM Remove the debit adjustment filter , it's not applicable 
*B602666,1 AMM on the credit file
IF ASCAN(laTOGVR,"IIF(TRANTYPE='2',DEBIT.TRANCODE,'')") # 0
  laTOGVR[ASUBSCRIPT(laTOGVR,ASCAN(laTOGVR,"IIF(TRANTYPE='2',DEBIT.TRANCODE,'')"),1),6]=''
ENDIF
lnCrAdjP = ASCAN(laTOGVR,"IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')")
IF lnCrAdjP # 0
  lnCrAdjP = ASUBSCRIPT(laTOGVR,lnCrAdjP,1)
  laTOGVR[lnCrAdjP,1] = "IIF(TRANTYPE='5',CREDIT.CCREDITCOD,laTOGVR[lnCrAdjP,6])"
ENDIF
lnPyFltP = ASCAN(laTOGVR,"IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')")
IF lnPyFltP # 0
  lnPyFltP = ASUBSCRIPT(laTOGVR,lnPyFltP,1)
  laTOGVR[lnPyFltP,1] = "IIF(TRANTYPE='4',CREDIT.CARPTYPE, laTOGVR[lnPyFltP,6])"
ENDIF
lcTOGVr=gfGenFlt('laTOGVR' , .T. , .T.)
lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFxFlt) AND !EMPTY(lcTOGVR),lcOGFxFlt+'|Ÿ',lcOGFxFlt)+;
           lcTOGVr+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFxFlt)AND !EMPTY(lcTOGVR),'Ÿ','')
IF EMPTY(lcOGFxFlt) AND EMPTY(lcTOGVr)
  lcRpExp=lcOGHDFlt
ELSE
  lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
          ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
ENDIF          
lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)        

*E605817,1 Modify lcRpExp. [Begin]
lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,lcNewSRExp)
*E605817,1 Modify lcRpExp. [End]

XFILTER = lcRpExp + lcRpFlt
*B602666,1 AMM end

*B603079,1 Add Currency Filter [Begin]
IF !EMPTY(lcCurrFilt)
  =lfAddCurr()
ENDIF
*B603079,1 Add Currency Filter [End  ]

IF lcrpCAB $ 'CB' .AND. lcrpCshT<>'B'
  DO CASE
    CASE lcrpCshT = 'A'
      XFILTER = XFILTER + ".AND.IIF(TranType='4',!lNonAR,.T.)"
    CASE lcrpCshT = 'N'
      XFILTER = XFILTER + ".AND.IIF(TranType='4',lNonAR,.T.)"
  ENDCASE
ENDIF
SELECT CREDIT
SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
COPY ALL TO (gcWorkDir+WORKFILE) FOR &XFILTER

SET TALK OFF

SELECT CREDIT
SET RELATION TO

*-- Add credit records to the main temporary file.
=gfOpenFile(gcWorkDir+WORKFILE,'','EX')
GO TOP
SCAN
  SCATTER MEMVAR MEMO
  m.TranCode = m.cCreditCod
  INSERT INTO (TRANFILE) FROM MEMVAR
ENDSCAN
USE

*--------------------------------------------
* COPY DEBIT DATA IF PROCESSING ADJUSTMENTS
*--------------------------------------------
IF lcrpCAB <> 'C'
  *B602666,1 AMM Form the filter expression, reForm the lcRpExp variable
  DIMENSION laTOGVR[1]
  =ACOPY(laOGVrFlt,laTOGVR)
  *B602666,1 AMM Remove the Credit adjustment filter, it's not applicable on 
  *B602666,1 AMM the debit file.
  IF ASCAN(laTOGVR,"IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')") # 0
    laTOGVR[ASUBSCRIPT(laTOGVR,ASCAN(laTOGVR,"IIF(TRANTYPE='5',CREDIT.CCREDITCOD,'')"),1),6]=''
  ENDIF

  IF ASCAN(laTOGVR,"IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')") # 0
    laTOGVR[ASUBSCRIPT(laTOGVR,ASCAN(laTOGVR,"IIF(TRANTYPE='4',CREDIT.CARPTYPE,'')"),1),6]=''
  ENDIF

  lcTOGVr=gfGenFlt('laTOGVR' , .T. , .T.)
  lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFxFlt) AND !EMPTY(lcTOGVR),lcOGFxFlt+'|Ÿ',lcOGFxFlt)+;
           lcTOGVr+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFxFlt)AND !EMPTY(lcTOGVR),'Ÿ','')
           
  IF EMPTY(lcOGFxFlt) AND EMPTY(lcTOGVr)
    lcRpExp=lcOGHDFlt
  ELSE
    lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
            ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
  ENDIF          
  lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)        
  *-- Remove the word 'Credit.' from the filter expression (THIS FILTER 
  *-- WILL BE USED ON debit AND arhist ALSO)
  lcRpExp = STRTRAN(UPPER(lcRpExp),'CREDIT.TRANDATE','TRANDATE')
  lcRpExp = STRTRAN(UPPER(lcRpExp),'CREDIT.BATCH','BATCH')
  *B602666,1 AMM end

  WAIT WINDOW 'SELECTING DEBITS FOR REPORT ...' NOWAIT
  SELECT DEBIT
  SET TALK ON
  *B602666,1 AMM Form the filter expression
  *XFILTER = lcRpExp

  *E605817,1 Modify lcRpExp. [Begin]
  lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,lcNewSRExp)
  *E605817,1 Modify lcRpExp. [End]

  XFILTER = lcRpExp + lcRpFlt
  *B602666,1 AMM end

  *B603079,1 Add Currency Filter [Begin]
  IF !EMPTY(lcCurrFilt)
    =lfAddCurr()
  ENDIF
  *B603079,1 Add Currency Filter [End  ]

  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER
  COPY ALL TO (gcWorkDir+WORKFILE) FOR &XFILTER
  SET TALK OFF
  SELECT DEBIT
  SET RELATION TO
  SELECT &TRANFILE
  APPEND FROM (gcWorkDir+WORKFILE)
ENDIF

SELECT &TRANFILE
GOTO TOP
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*B802253,1 BWA Adding the date to the report header 
*XTITLE = SPACE(0)
lnDatePos = ASCAN(laOGFxFlt,"CREDIT.TRANDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)

  DO CASE
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        XTITLE   = ''
      
    CASE EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,10))) .AND. !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],12,21)))
        Hdata     = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE    = "PERIOD: To &Hdata "
        
    CASE !EMPTY(CTOD(laOGFxFlt[lnDatePos,6]))
        Ldata    = SUBSTR(laOGFxFlt[lnDatePos,6],1,10)
        Hdata    = SUBSTR(laOGFxFlt[lnDatePos,6],12,21)
        XTITLE   = "PERIOD FROM: &Ldata To &Hdata "
  ENDCASE
ENDIF
*B802253,1 BWA [END]

DO CASE
  * CUSTOMER
  CASE lcRpSRTCD = 'C'
    XSORT   = 'ACCOUNT+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'ACCOUNT'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITLE  = XTITLE + " (SORT: CUSTOMER)"

  * TRANSACTION NUMBER
  CASE lcRpSRTCD = 'T'
    XSORT   = 'TRANTYPE+TRANCODE+DTOS(TRANDATE)+TRAN'
    HBREAK2 = 'TRANTYPE'
    HBREAK1 = 'TRANCODE'
    XTITLE  = XTITLE + " (SORT: TRANSACTION)"
  
  * BATCH NUMBER
  CASE lcRpSRTCD = 'B'
    XSORT   = 'BATCH+TRANTYPE+TRANCODE+TRAN'
    HBREAK2 = 'BATCH'
    HBREAK1 = 'TRANTYPE+TRANCODE'
    XTITL  = XTITLE + " (SORT: BATCH)"
  
  *E301356,1  Add a new option of sorting in case of adjustment only. [ Begin ]
  CASE lcRpSRTCD = 'R'
    XSORT   = 'TRANCODE+TRANTYPE+TRAN'
    HBREAK2 = 'TRANCODE'
    HBREAK1 = 'TRANCODE+TRANTYPE'
    XTITLE  = XTITLE + " (SORT: REASON CODE)"
    *E301356,1 [ End ] 
ENDCASE

Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'SORTING &Z RECORDS ...' NOWAIT
SET TALK ON
INDEX ON &XSORT TAG &TRANFILE

SET TALK OFF
SET ORDER TO TAG &TRANFILE

SELECT &TRANFILE
XBREAK1 = &HBREAK1
XBREAK2 = &HBREAK2


*-------------------------------------------------------
* [REPORT]
*-------------------------------------------------------

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

*B603079,1 lnAmount : Variable used to print amount 
*B603079,1            instead of Amount field. [Begin]
lnAmount = 0
*B603079,1            instead of Amount field. [End  ]

DO WHILE .T.
   SELECT &TRANFILE
   IF ROW >= 55
      ROW = 0
      PAGENO = PAGENO+1
      DO RPT_HDR WITH 'ARCHADJ',XTITLE,R_WIDTH
      *B802767,1  Hide the Reason CODE from the report & Print Reason Desc. .[ Begin ]
      *B802767,1  & Adjust to fit the new option of Print Reason Desc.
      *@ 05,000 SAY 'TRAN#  DATE       BATCH  CODE    .....DESC.....  ACCT# STORE    ...........CUSTOMER........... REFERENCE                 AMOUNT'
      *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3.
      *Tran#  Date     Batch  .......Reason........... .....Desc...... Acct# Store    ......Customer...... Reference                 Amount
      *999999 xx/xx/xx 999999 xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxx xxxxx xxxxxxxx xxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxxxxxxxx 99999999.99
      *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
      *@ 05,000 SAY 'Tran#  Date       Batch  .......Reason........... .....Desc.....  Acct# Store    ......Customer...... Reference              Amount'
      *B604274,1 AMM replace the word 'store' with 'check'
      *@ 05,000 SAY'Tran#  Date     Batch  .......Reason........... .....Desc...... Acct# Store    ......Customer...... Reference                 Amount'

      *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
      *@ 05,000 SAY'Tran#  Date     Batch  .......Reason........... .....Desc...... Acct# Check#   ......Customer...... Reference                 Amount'
      @ 05,000  SAY'Tran#  Date     Batch  .......Reason........... .....Desc...... Acct# Check#   ......Customer...... Reference               Amount'
      *B604303,1 [END]

      *B604274,1 AMM end
      *B603410,1 NAD (End)
      *B802767,1 [ END ]

      *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
      *@ 06,000 SAY REPLICATE('-',132)
      @ 06,000 SAY REPLICATE('-',130)
      *B604303,1 [END]

      ROW = 7
   ENDIF

   LEVEL = ' '
   DO CASE
     CASE EOF()
       LEVEL = '3'
     CASE XBREAK2 <> &HBREAK2
       LEVEL = '2'
     CASE XBREAK1 <> &HBREAK1
       LEVEL = '1'
   ENDCASE

   *------------------------------------------------
   * [SUB-TOTALS]
   *------------------------------------------------
   LCNT = 1
   DO WHILE LCNT <= VAL(LEVEL)
      DO CASE
        CASE LCNT=1
          PDASH = ' '
         *B802767,1  Hide the Reason CODE from the report & Print Reason Desc. .[ Begin ]
         *B802767,1  & Adjust to fit the new option of Print Reason Desc.
          *@ ROW,116 SAY '------------'
          *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
          *@ ROW,120 SAY '------------'

          *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
          *@ ROW,121 SAY '-----------'
          @ ROW,119 SAY '-----------'
          *B604303,1 [END]

          *B603410,1 NAD (End)
          *B802767,1 [ End ]
          IF lcrpDS = 'S'
            PBREAK = '          TOTAL -->     '+ lcTrnCd + '  '+lcTrnDec
          ELSE
            PBREAK = '          TOTAL --> '
          ENDIF
          
        CASE LCNT=2
          PDASH = DASHLN
          @ ROW,00 SAY PDASH
          
          IF lcRpSRTCD='T'
            DO CASE
              CASE XBREAK2='2'
                PBREAK = '          SUBTOTAL => DEBIT ADJUSTMENTS'
              CASE XBREAK2='4'
                PBREAK = '          SUBTOTAL => PAYMENTS'
              CASE XBREAK2='5'
                PBREAK = '          SUBTOTAL => CREDIT ADJUSTMENTS'
              CASE XBREAK2='7'
                PBREAK = '          SUBTOTAL => ALLOWANCES'
            ENDCASE
          ELSE
            *E301356,1 Change the title of the subtotal in case of 
            *E301356,1 subtotal by reason. [ Begin ] 
            *PBREAK = '          SUBTOTAL => ' + XBREAK2
            IF lcRpSRTCD = 'R'
              PBREAK = '          SUBTOTAL => REASON CODE ' + lcTranCods
            ELSE
              PBREAK = '          SUBTOTAL => ' + XBREAK2
            ENDIF
            *E301356,1 [ End ]

          ENDIF

        CASE LCNT=3
          PDASH = DASHLN2
          @ ROW,00 SAY PDASH
          PBREAK = '          GRAND TOTAL'
      ENDCASE
      ROW = ROW+1

      ** PSTAR = REPLICATE('*', LCNT)
      @ ROW,000 SAY PBREAK
      *B802767,1 Hide the Reason CODE from the report & Print Reason Desc. .[ Begin ]
      *B802767,1  & Adjust to fit the new option of Print Reason Desc.
      *@ ROW,116 SAY TAMT(LCNT)   PICTURE '99999999.99'
      *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
      *@ ROW,120 SAY TAMT(LCNT)   PICTURE '99999999.99'
      *E301429,1 (Start)  Increase the amount field.
      *@ ROW,121 SAY TAMT(LCNT)   PICTURE '99999999.99'
      IF llRpDec
        *B604144,1 KAM 22/01/2001 [Begin] Increase the picture from 11,2 to 13,2   
        *@ ROW,121 SAY TAMT(LCNT)   PICTURE '99999999.99'

        *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
        *@ ROW,119 SAY TAMT(LCNT)   PICTURE '9999999999.99'
        @ ROW,117 SAY TAMT(LCNT)   PICTURE '9999999999.99'
        *B604303,1 [END]

        *B604144,1 KAM [End]
      ELSE

        *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
        *@ ROW,121 SAY TAMT(LCNT)   PICTURE '99999999999'
        @ ROW,119 SAY TAMT(LCNT)   PICTURE '99999999999'
        *B604303,1 [END]

      ENDIF    
      *E301429,1 (End)
      *B603410,1 NAD 
      *B802767,1 [ End ]
      ** @ ROW,123 SAY PSTAR
      ROW = ROW+1

      @ ROW,00 SAY PDASH
      ROW = ROW + IIF(LCNT=1, 1, 2)

      DO CASE
       CASE LCNT=1
         XBREAK1 = &HBREAK1
       CASE LCNT=2
         XBREAK2 = &HBREAK2
      ENDCASE

      IF LCNT < 3
         TAMT(LCNT+1) = TAMT(LCNT+1) + TAMT(LCNT)
         TAMT(LCNT)   = 0.00
      ENDIF
      LCNT = LCNT+1
   ENDDO

   IF LEVEL = '3'
      EXIT
   ENDIF

   IF LEVEL<>' ' .AND. ROW>50
      LOOP
   ENDIF

   SELECT &TRANFILE
   *B603079,1 Calculate Base Currency (if multi currency) [Begin
   lnAmount = lfBaseAmt()
   *B603079,1 Calculate Base Currency (if multi currency) [End..

   *B603079,1 Calculate totals depends on lnAmount [Begin
   *TAMT(1) = TAMT(1) + AMOUNT
   TAMT(1) = TAMT(1) + lnAmount
   *B603079,1 Calculate totals depends on lnAmount [End..

   SELECT &TRANFILE
   IF lcrpDS = 'S'
     lcTrnCd  = TRANCODE
     lcTrnDec = SUBSTR(DESC,1,15)
     SKIP
     LOOP
   ENDIF

   XACCOUNT = 'M'+ACCOUNT
   SELECT CUSTOMER
   SEEK XACCOUNT   
   SELECT &TRANFILE
   @ ROW,000 SAY TRAN
   @ ROW,007 SAY TRANDATE
   *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
   *@ ROW,018 SAY BATCH
    @ ROW,016 SAY BATCH
   *B603410,1 NAD (End) 
   *B802767,1 ABD Hide the Reason CODE from the report & Print Reason Desc. .[ Begin ]
   *B802767,1  & Adjust to fit the new option of Print Reason Desc.
   *@ ROW,025 SAY TRANCODE
   *@ ROW,033 SAY LEFT(DESC,15)
   *@ ROW,049 SAY ACCOUNT
   *@ ROW,055 SAY STORE
   *@ ROW,064 SAY CUSTOMER->BTNAME
   *@ ROW,095 SAY LEFT(REFERENCE,20)
   
   IF EMPTY(TRANCODE)
     lcTranCods = ''
   ELSE
     IF TRANTYPE = '2'
       lcTranCods =GFCODDES(TRANCODE,'TRANCODE')
     ELSE    
       lcTranCods =GFCODDES(TRANCODE,'CCREDITCOD')
     ENDIF
   ENDIF
    *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
    *@ ROW,025 SAY LEFT(lcTranCods,24)
    *@ ROW,050 SAY SUBSTR(DESC,1,15)
    *@ ROW,066 SAY ACCOUNT
    *@ ROW,072 SAY STORE
    *@ ROW,081 SAY SUBSTR(CUSTOMER.BTNAME,1,20)
    *@ ROW,102 SAY SUBSTR(REFERENCE,1,20)
    
    @ ROW,023 SAY LEFT(lcTranCods,24)
    @ ROW,048 SAY SUBSTR(DESC,1,15)
    @ ROW,064 SAY ACCOUNT
    @ ROW,070 SAY STORE
    @ ROW,079 SAY SUBSTR(CUSTOMER.BTNAME,1,20)

    *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
    *@ ROW,100 SAY SUBSTR(REFERENCE,1,20)
    @ ROW,100 SAY SUBSTR(REFERENCE,1,18)
    *B604303,1 [END]

   *B603410,1 NAD (End)
   *B802767,1 [ End ]
   
   *B603079,1 Print lnAmount istead of Amount Field [Begin
   *@ ROW,116 SAY AMOUNT  PICTURE '99999999.99'
   *B802767,1  Hide the Reason CODE from the report & Print Reason Desc. .[ Begin ]
   *B802767,1  & Adjust to fit the new option of Print Reason Desc.
   *@ ROW,116 SAY lnAmount  PICTURE '99999999.99'
   *B603410,1 NAD 01/31/2000 (Start) Fix bug of Adjust column width in Amount column 
   * @ ROW,116 SAY lnAmount  PICTURE '999999.99'  
   
   *E301429,1 (Start)  Increase the amount field.
   *@ ROW,121 SAY lnAmount  PICTURE  '99999999.99'  
   IF llRpDec

     *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]   
     *@ ROW,121 SAY lnAmount  PICTURE  '99999999.99'
     @ ROW,119 SAY lnAmount  PICTURE  '99999999.99'
     *B604303,1 [END]

   ElSE

     *B604303,1 BWA 03/20/2001 Move the amount two characters left.[START]
     *@ ROW,121 SAY lnAmount  PICTURE  '99999999999'
     @ ROW,119 SAY lnAmount  PICTURE  '99999999999'
     *B604303,1 [END]

   ENDIF   
   *E301429,1 (End)
   *B603410,1 NAD (End)
   *B802767,1 [ End ]
   *B603079,1 Print lnAmount istead of Amount Field [End..
    ROW = ROW+1
   PCODESC=SUBSTR(DESC,1,15)
   SELECT &TRANFILE
   SKIP
ENDDO

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN

IF USED(TRANFILE)
  USE IN (TRANFILE)
  ERASE (gcWorkDir+TRANFILE+'.DBF')
  ERASE (gcWorkDir+TRANFILE+'.CDX')
ENDIF
IF USED(WORKFILE)
  USE IN (WORKFILE)
  ERASE (gcWorkDir+WORKFILE+'.DBF')
ENDIF

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/30/1998
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

*E605817,1 Get position of Sales Rep. [Begin]
lnSlRepPos = lfItmPos('CUSTOMER.SALESREP')          && Sales Rep Position
*E605817,1 Get position of Sales Rep. [End]

*-- Get the position of the print cost setting in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcrpCshT'),1)
laOGObjCnt[lnPos] = (lcrpCAB<>'A')
lcrpCshT = 'B'
= lfOGShowGet('lcrpCshT')

*B603079,1 Add Multi currency format to the report. [Begin
*-- Company use Multi Currency.
IF llMultCurr
  lnCurrPos  = lfGetVarPos("lcRpCurr")  && Currency position.

  *-- if it is User Saved Filter and your saving is to print by
  *-- equavelent in base amount.
  IF !(lnOGSeting = 1 OR llRpForCur)
    =lfvForCurr()
  ENDIF

ELSE  && Company use single currency
  *-- Rest Currency variables.
  lcRpCurr   = ""
  llRpForCur = .F.  
ENDIF  
*B603079,1 Add Multi currency format to the report. [End..

*E301356,1 ABD Define Variable sort With the Defa value. [ Begin ]
lcRpSRTCD = "T"
*E301356,1 [ End ]

*-- end of lfwOGWhen.

*!*************************************************************
*! Name      : lfvCAB
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/30/1998
*! Purpose   : Valid function of CASH & ADJUSTMENTS setting in the 
*!             option grid to enable or disable Cash type setting 
*!             (AR or NON AR or BOTH)
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCAB()
*!*************************************************************
FUNCTION lfvCAB
PRIVATE lnArreyLen
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcrpCshT'),1)
laOGObjCnt[lnPos] = (lcrpCAB<>'A')
lcrpCshT = 'B'

= lfOGShowGet('lcrpCshT')

lnArreyLen = ALEN(laDataVal,1)
*E301356,1 ABD 30/11/1999 Add a new option of sorting in case of adjustment only. [ Begin ]
IF lcrpCAB = "A"
  DIMENSION laDataDes[4,1],laDataVal[4,1]
  laDataDes[1] = "Transaction"
  laDataDes[2] = "Customer"
  laDataDes[3] = "Batch"
  laDataDes[4] = "Reson Code"
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
  laDataVal[4] = "R"
ELSE
  DIMENSION laDataDes[3,1],laDataVal[3,1]
  laDataDes[1] = "Transaction"
  laDataDes[2] = "Customer"
  laDataDes[3] = "Batch"
  laDataVal[1] = "T"
  laDataVal[2] = "C"
  laDataVal[3] = "B"
ENDIF

IF lnArreyLen != ALEN(laDataVal,1)
  lcRpSRTCD = "T"
  CLEAR READ
ENDIF  
*E301356,1 ABD [ End ]

*!*************************************************************
*! Name      : lfvCashTyp
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/28/1998
*! Purpose   : To clear the selection of the in range function of the account
*!*************************************************************
*! Called from : Option Grid from two setting (Cash Type, Account Type)
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCashTyp()
*!*************************************************************
FUNCTION lfvCashTyp
*- Variable To Clear Selection From The InRang If Disable The Bottom.
llClear = .T.

*E301356,1 ABD 30/11/1999 Add a new option of sorting in case of adjustment only.

*-- Get the position of the Customer in the array to enable or 
*-- disable it.

IF ASCAN(laOGFxFlt,"CUSTOMER.ACCOUNT") # 0
  *E301356,1 ABD Get the number of the source location element in the variable filter array 
  lnSrcLoc   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"CUSTOMER.ACCOUNT"),1)
  *E301356,1 ABD Enable the object.
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGVrFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")
  
  *E301356,1 ABD Refresh the source location setting in the option grid
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSrcLoc)) + ',6]')
ENDIF

*E605817,1 Enable or Disbale SalesRep. [Begin]
IF ASCAN(laOGFxFlt,"CUSTOMER.SALESREP") # 0
  lnSrcLoc   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"CUSTOMER.SALESREP"),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) - ALEN(laOGVrFlt,1)+ lnSrcLoc] =  (lcrpCshT#"N")
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSrcLoc)) + ',6]')
ENDIF
*E605817,1 Enable or Disbale SalesRep. [End]

*E301356,1 ABD 30/11/1999 Add a new option of sorting in case of adjustment only.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/30/1998
*! Purpose   : The reset function of the in range function of the account
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'R'  && Reset code
  llClear = .F.
ENDIF

*!*************************************************************
*! Name      : lfFillCurr
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/02/1999
*! Purpose   : Fill Currency arrays
*!*************************************************************
*! Example     : = lfFillCurr()
*!*************************************************************
*B603079,1
FUNCTION lfFillCurr
llMultCurr = gfGetMemVar('llMulCurr')
IF llMultCurr
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]

ENDIF
*-- end of lfFillCurr.

*!*************************************************************
*! Name      : lfGetVarPos
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/02/1999
*! Purpose   : Evaluate variable position.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B603079,1
FUNCTION lfGetVarPos
PARAMETERS lcVarName
PRIVATE lnVarPos
lnVarPos = ASCAN(laOGObjType,lcVarName)
IF lnVarPos > 0
  lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
ENDIF
RETURN lnVarPos
*-- end of lfItmPos.

*!*************************************************************
*! Name      : lfvForCurr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/02/1999
*! Purpose   : Print in Foreign currency (Y/N)
*!*************************************************************
*! Example   : = lfvForCurr()
*!*************************************************************
*B603079,1
FUNCTION lfvForCurr
*-- Disable if print in base currency else enable it.
lcRpCurr = gcBaseCurr
laOGObjCnt[lnCurrPos] = llRpForCur
=lfOGShowGet("lcRpCurr")
*-- end of lfvForCurr.

*!*************************************************************
*! Name      : lfAddCurr
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/02/1999
*! Purpose   : Add Currency filter
*!*************************************************************
*! Example   : = lfAddCurr()
*!*************************************************************
*B603079,1
FUNCTION lfAddCurr
XFILTER = ALLTRIM(XFILTER)
IF !EMPTY(XFILTER)
  XFILTER = XFILTER + [ AND ]
ENDIF
XFILTER = XFILTER + lcCurrFilt
*-- end of lfAddCurr.

*!*************************************************************
*! Name      : lfBaseAmt
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/02/1999
*! Purpose   : Compute base amount
*!*************************************************************
*! Example   : = lfBaseAmt()
*!*************************************************************
*B603079,1
FUNCTION lfBaseAmt
PRIVATE lnBaseAmt
lnBaseAmt = AMOUNT
*-- if Multi currency and user want to print in base currency and 
*-- currency not the base currency.
IF llMultCurr AND !llRpForCur AND (cCurrCode != gcBaseCurr)
  lnBaseAmt = gfAmntDisp(lnBaseAmt,"O",TranDate)
ENDIF
RETURN lnBaseAmt
*-- end of lfBaseAmt.

*!***********************************************************************
*! Name        :  lfFilSortb
*! Developer   : ABDOU El-GENDI (ABD)
*! Date        : 11/30/1999
*! Purpose     : Add a new option of sorting in case of adjustment only.
*!***********************************************************************
*! Called from : Option Grid
*!***********************************************************************
*! Example     :=lfFilSortb()
*!***********************************************************************
*E301356,1
Function lfFilSortb

DIMENSION laDataDes[3,1],laDataVal[3,1]
laDataDes[1] = "Transaction"
laDataDes[2] = "Customer"
laDataDes[3] = "Batch"

laDataVal[1] = "T"
laDataVal[2] = "C"
laDataVal[3] = "B"
*-- End Of lfFilSortb.
*!*************************************************************

*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/21/2002
*! Purpose   : Go top in Sales Rep file.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSerSRep()
*!**************************************************************************
*E605817,1
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

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/21/2002
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
*E605817,1
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.
