*:***************************************************************************
*: Program file  : ARCSCROG.PRG
*: Program desc. : CUSTOMIZED CUSTOMER CREDIT REPORT.
*: 				   The program is a conversion from 2.6 ..OGI600.PRG (REFERE TO C100976)
*! Date          : 05/11/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101858
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARCSCROG
*:***************************************************************************
              *--Variables in the hidden filter in report generator--*
*1) In hidden filter : Customer.Type = "M"
              
              *--Initial the variables--*
lcAgingTyp = ALLTRIM(GFGETMEMVAR('XAGINGTYPE'))       && the aging type
                                                      && lcAgingTyp = "D"  by date , lcAgingTyp = "T" by Terms

WAIT WINDOW 'SELECTING RECORDS FOR THE REPORT ...' NOWAIT
SELE CUSTOMER
SET FILTER TO &lcRpexp
GOTO TOP

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*-- Asking if no records (Display message) otherwise print report [End.]

              *--Section print the report--*
ROW     = 99
PAGENO  = 0
R_WIDTH ='W'
R_TITLE = 'CUSTOMIZED CUSTOMER CREDIT REPORT'

WAIT WINDOW 'Report printing press <SPACE BAR> to abort ' NOWAIT
SET DEVICE TO PRINT

SCAN WHILE !EOF()
  llNothing = IIF(ROW >= 53,lfPrnHdr(),.F.)
  @ ROW,00 SAY 'ACCOUNT #          : ' + Account
  @ ROW,55 SAY 'NAME   : '+ BtName
  ROW = ROW + 1
  @ ROW,00 SAY 'ACCT. OPENED DATE  : '+DTOC(DAdd_Date)
  @ ROW,55 SAY 'CURRENT: '
  lcCurrent = IIF(lcAgingTyp='D',Current,tercurrent)
  @ ROW,64 SAY ALLTRIM(STR(lcCurrent,11,2))
  ROW = ROW + 1
  @ ROW,00 SAY 'TERMS              : '+ gfCodDes(CUSTOMER.CTERMCODE,'CTERMCODE')
  @ ROW,55 SAY '+1 -30 : '
  lcAge30 = IIF(lcAgingTyp='D',age30,terage30)
  @ ROW,64 SAY ALLTRIM(STR(lcAge30,11,2))
  ROW = ROW + 1
  @ ROW,00 SAY 'AVERAGE DAYS TO PAY: '

  lnAvgDays = 0             && know the AVERAGE DAYS for paying.
  lnNoOfInv = 0             && know the number of the invoices.
  SELECT ArHist
  IF SEEK(Customer.Account)
    SUM REST (HistDate-TranDate), 1 TO lnAvgDays, lnNoOfInv;
             WHILE Account=Customer.Account FOR TranType='1'
  ENDIF
  SELECT Customer
  @ ROW,21 SAY IIF(lnNoOfInv<>0,ALLTRIM(STR(ROUND(lnAvgDays/lnNoOfInv,0))),0)
  @ ROW,55 SAY '+31-60 : '
  lcAge60 = IIF(lcAgingTyp='D',age60,terage60)
  @ ROW,64 SAY ALLTRIM(STR(lcAge60,11,2))
  ROW = ROW + 1
  @ ROW,00 SAY 'CREDIT LIMIT       : '
  @ ROW,21 SAY ALLTRIM(STR(CrLimit,7))
  @ ROW,55 SAY '+61-90 : '
  lcAge90 = IIF(lcAgingTyp='D',age90,terage90)
  @ ROW,64 SAY ALLTRIM(STR(lcAge90,11,2))
  ROW = ROW + 1
  @ ROW,00 SAY 'AMOUNT NOW DUE     : '
  @ ROW,21 SAY ALLTRIM(STR(NetBal,11,2))
  @ ROW,55 SAY '+90    : '
  lcAge120 = IIF(lcAgingTyp='D',age120,terage120)
  @ ROW,64 SAY ALLTRIM(STR(lcAge120,11,2))
  ROW = ROW + 1
  @ ROW,00 SAY REPLICATE ('*',132)
  ROW = ROW + 1
ENDSCAN
DO ENDREPORT
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/11/2000
*! Purpose   : To print the report's header.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrnHdr()
*!*************************************************************
FUNCTION lfPrnHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'CUSTOMER CREDIT REPORT','',R_WIDTH
ROW = 5
*!**************************************************************************
*! Name      : lfvAcct
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/11/2000
*! Purpose   : Validation function for the Customer Account field.
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAcct()
*!**************************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.
  lcAccount = lcObjVal
  DO CUSBROWM WITH lcAccount
  lcObjVal = lcAccount
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal
*-- End of lfvAcct.
