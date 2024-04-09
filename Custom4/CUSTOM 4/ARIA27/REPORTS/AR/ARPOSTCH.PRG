*:***************************************************************************
*: Program file  : ARPOSTCH
*: Program desc. : Post Dated Checks
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : Hossam El Etreby (HDM)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO ARPOSTCH
*:***************************************************************************
*B603713,5 NAD 10/02/2000 Increase the picture of the amount Field.  
*B606421,1 SSE 10/21/2002 Fix bug of feature not available when pressing Run while having no records.
*B123423,1 NNA 08/08/2004 Fix bug that if you select an account that has a Post dated Cheques you 
*B123423,1 NNA            will get massage 'No records to Display' but if you don't specify a range
*B123423,1 NNA            this account will appear on the report
*:***************************************************************************
STORE ''       TO lcWhile,lcFor
lnAccPos   = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSTDCHQ.PAYDATE'),1)

*-- Get Account Values
lcAccVal = laOGFxFlt[lnAccPos,6]
lnSepPos = ATC('|',lcAccVal)
lcLAccnt = SUBSTR(lcAccVal , 1 , lnSepPos - 1)
lcHAccnt = SUBSTR(lcAccVal , lnSepPos + 1 ,LEN(lcAccVal))
*--

*-- Pay Date range
lcDateVal = laOGFxFlt[lnDatePos , 6]
lnSepPos  = ATC('|' , lcDateVal)
ldLPDate  = CTOD(SUBSTR(lcDateVal , 1 , lnSepPos - 1))
ldHPDate  = CTOD(SUBSTR(lcDateVal , lnSepPos + 1 ,LEN(lcDateVal)))
*--
*-Prepare Filter
SELECT PostDChq
*-- HDM Fixing Bug Of incorrect filter passed

DO CASE
  CASE !EMPTY(lcLAccnt) .AND. !EMPTY(lcHAccnt)
    lcWhile = "BETWEEN(ACCOUNT , lcLAccnt , lcHAccnt)"
  
  *B123423,1 NNA 08/08/2004 (Begin) if the user didn't input a first Account and input only the second
  CASE EMPTY(lcLAccnt) .AND. !EMPTY(lcHAccnt)
    lcWhile = "ACCOUNT <= lcHAccnt"
  *B123423,1 NNA (End)

  OTHERWISE
      lcWhile = ".T."
ENDCASE
*IF !EMPTY(lcLAccnt)
*  =SEEK(lcLAccnt)
*  lcWhile="Account<=lclAccnt"
*  
*  IF !EMPTY(lcHAccnt)
*    =SEEK(lcHAccnt)
*    lcWhile = lcWhile + ' .AND. ' + "Account >= lcHAccnt"
*  ENDIF
*ELSE
*  GO TOP
*  lcWhile=".T."
*ENDIF

IF !EMPTY(ldLPDate)
  lcFor ="BETWEEN(PayDate,ldLPDate,ldHPDate)"
ELSE
  lcFor =".T."
ENDIF
GO TOP

*B123423,1 NNA 08/08/2004 (Begin) Fix Locate Statement
*LOCATE REST WHILE &lcWhile FOR &lcFor
LOCATE FOR &lcWhile .AND. &lcFor
*B123423,1 NNA (End)

IF !FOUND()
  
  *B606421,1 Change the type of message. [Begin] 
  *=gfDialog('I','No records found,Cannot proceed.')  
  = gfModalGen('TRM00052B00000','DIALOG' )
  *B606421,1 Change the type of message. [End]

  SET DEVICE TO SCREEN  
  RETURN (.F.)
ENDIF



*-- Begin printing loop.
PageNo  = 0
Row     = 99
R_Width = 'N'
R_Title = "CUSTOMER POSTDATED Checks LIST"

CLEAR TYPEAHEAD
SET DEVICE TO PRINT

*--HDM
SELECT PostDChq
*--HDM

*IF !EMPTY(lcLAccnt)
*  =SEEK(lcLAccnt)
*ELSE
*  GO TOP
*ENDIF

*-Init. Totals
lcAccount = Account
lcAccDesc = IIF(SEEK('M'+lcAccount,'CUSTOMER'),CUSTOMER.BtName,'')
STORE 0.00 TO lnSubTot,lnGrnTot

SCAN WHILE &lcWhile .AND. INKEY()<>32 FOR &lcFor
*--HDM
lcAccount = Account
lcAccDesc = IIF(SEEK('M'+lcAccount,'CUSTOMER'),CUSTOMER.BtName,'')


*  SET DEVICE TO SCREEN
*  @ 23,00 SAY Account COLOR &qClrSay
*  SET DEVICE TO PRINT
*--HDM
  IF ROW >=57
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ARPOSTCH','',R_Width
    *B603713,5 NAD 10/02/2000 (Begin)
    *@ 05,00 SAY ' ACCOUNT DESCRIPTION...................  PAY DATE  Check NO.       AMOUNT'
    @ 05,00 SAY ' ACCOUNT DESCRIPTION...................  PAY DATE  Check NO.           AMOUNT'
    *B603713,5 NAD 10/02/2000 (End)
    @ 06,00 SAY REPLICATE('*',80)
    ROW=07
  ENDIF
  *-- Stotals.
  IF Account<>lcAccount
    @ ROW,00 SAY REPLICATE('-',80)
    ROW=ROW+1    
    @ ROW,00 SAY '*** SUBTOTAL *** '+lcAccount
    @ ROW,63 SAY lnSubTot PICTURE '99999999.99' 
    ROW=ROW+1    
    @ ROW,00 SAY REPLICATE('-',80)
    ROW=ROW+1    
    lcAccount = Account
    lcAccDesc = IIF(SEEK('M'+lcAccount,'CUSTOMER'),CUSTOMER.BtName,'')
    lnGrnTot  = lnGrnTot + lnSubTot
    lnSubTot  = 0
  ENDIF

  @ ROW,01 SAY Account
  @ ROW,09 SAY lcAccDesc
  @ ROW,41 SAY PayDate
  @ ROW,51 SAY ChequeNo
  *B603713,5 NAD 10/02/2000 (Begin) Increase the picture of the amount Field.  
  *@ ROW,64 SAY Amount PICTURE '9999999.99' 
  @ ROW,64 SAY Amount PICTURE '9999999999.99' 
  *B603713,5 NAD 10/02/2000 (End)
  
  *- Accomulate subtotals
  lnSubTot =lnSubTot + Amount
  ROW=ROW+1

ENDSCAN
*-- Last subtotal.
@ ROW,00 SAY REPLICATE('-',80)
ROW=ROW+1    
@ ROW,00 SAY '*** SUBTOTAL *** '+lcAccount
*B603713,5 NAD 10/02/2000 (Begin) Increase the picture of the amount Field.  
*@ ROW,63 SAY lnSubTot PICTURE '99999999.99' 
@ ROW,63 SAY lnSubTot PICTURE '99999999999.99' 
*B603713,5 NAD 10/02/2000 (End)
ROW=ROW+1    
@ ROW,00 SAY REPLICATE('-',80)
ROW=ROW+1    
lnGrnTot  = lnGrnTot + lnSubTot
lnSubTot  = 0

*-- Print Grand Totals.
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('=',80)
ROW=ROW+1    
@ ROW,00 SAY '*** GRAND TOTAL *** '
*B603713,5 NAD 10/02/2000 (Begin) Increase the picture of the amount Field.  
*@ ROW,63 SAY lnGrnTot PICTURE '99999999.99' 
@ ROW,63 SAY lnGrnTot PICTURE '99999999999.99' 
*B603713,5 NAD 10/02/2000 (End)
ROW=ROW+1    
@ ROW,00 SAY REPLICATE('=',80)

*-- HDM DO ENDPRT
DO ENDREPORT

SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby
*! Date      : 08/17/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
