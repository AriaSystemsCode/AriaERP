*:***************************************************************************
*: Program file  : ARKRA140.PRG
*: Program desc. : FACTOR COMMISSION DUE REPORT.
*: For Report    : ......
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : Krazy Kat Ltd.
*:***************************************************************************
*: C101636,1 KHM 10/19/1999
*:***************************************************************************
*:Modifications ..
*:***************************************************************************
*:B606630,1 RAE 11/14/2002 Recalculate the Terms Discount Amount field.
*:***************************************************************************
*
*-- To check if the factor was empty do not continue.
IF EMPTY(lcRpFactor)
  *=gfModalGen('TRM40164B00000','DIALOG' )
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Factor code should not be empty. Cannot proceed')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Initializing the global variables
XTITLE     = ALLTRIM(SycFact.cFacComp)
InvTemp    = gfTempName()

*-- Saving the factor's commission
SAVE TO gcDataDir+"KRA1400.MEM" ALL LIKE lnRpFComm

*-- Printing the report.
=lfPrnRept()

*!*************************************************************
*! Name      : lfPrnRept
*! Developer : KHALD MOHI EL-DIN
*! Date      : 10/19/1999
*! Purpose   : Function to filter the InvHdr file and print the 
*!             report
*!**************************************************************
*! Layout :
*!0----*----1----*----2----*----3----*-----4----*-----5----*----6----*----7----*----8----*----9----*----0----*----1----*----2----*----3--
*!          ----- Document ------        Comm-amt        Net Sale                            Gross Sale       Terms disc
*! Cust     Inv.#   Typ  Date              due             amount     Ord-#     Pick-#           amount           amount     
*!--------------------------------------------------------------------------------------------------------------------------------------
*! XXXXX    999999   I   mm/dd/yy       999999.99    9,999,999.99    999999     999999     9,999,999.99     9,999,999.99
*! XXXXX    999999   I   mm/dd/yy       999999.99    9,999,999.99    999999     999999     9,999,999.99     9,999,999.99
*! XXXXX    999999   I   mm/dd/yy       999999.99    9,999,999.99    999999     999999     9,999,999.99     9,999,999.99
*! Cust: xxxxxxxxxxxxxxxxxxxx totals :  999999.99    9,999,999.99                          9,999,999.99     9,999,999.99
*!*************************************************************************************************************************************
*! Example            :  lfPrnRept()
*!*************************************************************************************************************************************
FUNCTION lfPrnRept
PRIVATE lcFilter, llMsg

*-- initializing the necessary variables
ROW       = 99
PAGENO    = 0
R_TITLE   = 'FACTOR COMMISSIONS REPORT'

STORE 0 TO lnSubComm, lnSubNet, lnSubTot, lnSubTrde,;
           lnGrdComm, lnGrdNet, lnGrdTot, lnGrdTrde

lcFilter = "cFacCode = lcRpFactor" + lfDateSele() + lfAccSele()

SELECT InvHdr
GOTO TOP

COPY TO &gcWorkDir.&Invtemp  FOR &lcFilter
= gfOpenFile ('&gcWorkDir.&Invtemp',' ','EX')
INDEX ON Account TAG (Invtemp)

SELECT(InvTemp)
GOTO TOP
IF RECCOUNT(InvTemp) = 0
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT
llFirst = .T.
lcAcc   = SPACE(5)

SCAN FOR &lcFilter AND INKEY() <> 32
  IF llFirst
    STORE .F. TO llFirst
    lcAcc     = Account
    llNothing = lfPrtHdr()    
    @ ROW,00 SAY 'Cust:'
    IF SEEK('M'+lcAcc, 'Customer')
      @ ROW,06 SAY SUBSTR(Customer.BtName,1,20)
    ENDIF   
    ROW = ROW + 2
  ENDIF

  IF Account <> lcAcc  
    = IIF(Row > 57, lfPrtHdr(), .F.)
    lcAcc = Account
    *-- Function to print the subtotal per customer
    = lfPrSubGrd("S",lnSubComm,lnSubNet,lnSubTot,lnSubTrde)
    STORE 0 TO lnSubComm, lnSubNet, lnSubTot, lnSubTrde
  ENDIF
  
  = IIF(Row > 57, lfPrtHdr(), .F.)
  @ ROW,00 SAY Account
  @ ROW,09 SAY Invoice
  @ ROW,18 SAY 'I'
  @ ROW,22 SAY InvDate

  *-- Net Sale amount = (total charges - trade discount of the invoice)
  lnNetAmnt  = TotalChg - Trde_Disc
  lnNetAmnt  = ROUND(lnNetAmnt,2)
  *-- Commissions amount due = (factor comm * Net sale amount) / 100
  lnCommAmnt = (lnRpFComm * lnNetAmnt) / 100
  lnCommAmnt = ROUND(lnCommAmnt,2)
  @ ROW,037 SAY lnCommAmnt PICTURE '999999.99'
  @ ROW,050 SAY lnNetAmnt  PICTURE '9,999,999.99'
  @ ROW,067 SAY Order
  @ ROW,077 SAY PikTkt
  @ ROW,088 SAY TotalChg   PICTURE '9,999,999.99'
  
  *B606630,1 RAE Recalculate the Terms Discount Amount field. [start]
  *@ ROW,105 SAY Trde_Disc  PICTURE '9,999,999.99'
  lnTrmDisc = (TotalChg * Trde_Disc) / 100  
  @ ROW,105 SAY lnTrmDisc  PICTURE '9,999,999.99'
  *B606630,1 RAE [end]
  
  ROW = ROW + 1
        
  *-- Function to accumolate the subtatals and the grand totals
  =lfAccumlat() 
ENDSCAN

= IIF(Row > 57, lfPrtHdr(), .F.)
*-- To print the subtotal for the last record
= lfPrSubGrd("S",lnSubComm,lnSubNet,lnSubTot,lnSubTrde)
*-- Function to print the grand total for all the customers
= lfPrSubGrd("",lnGrdComm ,lnGrdNet,lnGrdTot,lnGrdTrde)
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfPrtHdr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To print the header of the report.
*:*************************************************************
*! Example     : =lfPrtHdr()
*!*************************************************************
FUNCTION lfPrtHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'ARKRA140',XTITLE,R_WIDTH
ROW = 5
@ ROW,00 SAY '         ----- Document ------        Comm-amt        Net Sale                            Gross Sale       Terms disc              '
ROW = ROW + 1
@ ROW,00 SAY 'Cust     Inv.#   Typ  Date              due             amount     Ord-#     Pick-#           amount           amount              '
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',132)
ROW = ROW + 1

*!*************************************************************
*! Name      : lfAccumlat
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To accumolate the subtotal per customer and the grand total
*!             for all the report.
*:*************************************************************
*! Example     : =lfAccumlat()
*!*************************************************************
FUNCTION lfAccumlat

*-- This function will accumolating these fields : 
*-- lnCommAmnt, lnNetAmnt, TotalChg, and Trde_Disc
lnSubComm = lnSubComm + lnCommAmnt
lnSubNet  = lnSubNet  + lnNetAmnt
lnSubTot  = lnSubTot  + TotalChg

*B606630,1 RAE Recalculate the Terms Discount Amount field in Sub Total.[start]
*lnSubTrde = lnSubTrde + Trde_Disc
lnSubTrde = lnSubTrde + lnTrmDisc
*B606630,1 RAE [end]

lnGrdComm = lnGrdComm + lnCommAmnt
lnGrdNet  = lnGrdNet  + lnNetAmnt
lnGrdTot  = lnGrdTot  + TotalChg

*B606630,1 RAE Recalculate the Terms Discount Amount field in Grand Total. [start]
*lnGrdTrde = lnGrdTrde + Trde_Disc
lnGrdTrde = lnGrdTrde + lnTrmDisc
*B606630,1 RAE [end]

*!*************************************************************
*! Name      : lfPrSubGrd
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To print the subtotals and the Grand totals for these
*!             fields lnCommAmnt,lnNetAmnt,TotalChg,Trde_Disc
*:*************************************************************
*! Example     : =lfPrSubGrd()
*!*************************************************************
FUNCTION lfPrSubGrd
PARAMETERS lcType,lnField1, lnField2, lnField3, lnField4

ROW = ROW + 1
IF lcType = 'S'
  @ ROW,28 SAY 'Totals: '
ELSE
  @ ROW,20 SAY 'Grand Totals :'  
ENDIF

@ ROW,037 SAY lnField1 PICTURE '999999.99'
@ ROW,050 SAY lnField2 PICTURE '9,999,999.99'
@ ROW,088 SAY lnField3 PICTURE '9,999,999.99'
@ ROW,105 SAY lnField4 PICTURE '9,999,999.99'

IF lcType = 'S' AND !EOF(InvTemp)
  ROW = ROW + 1
  @ ROW,00 SAY 'Cust:'
  IF SEEK('M'+lcAcc, 'Customer')
    @ ROW,06 SAY SUBSTR(Customer.BtName,1,20)
  ENDIF  
ENDIF
ROW = ROW + 2

*!*************************************************************
*! Name      : lfDateSele
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To select the date range and put them in the filter
*:*************************************************************
*! Example     : =lfDateSele()
*!*************************************************************
FUNCTION lfDateSele

*-- Fill the lcSelect variable according to the grid options.
DO CASE
  CASE !EMPTY(ldLwInvDt) .AND.  EMPTY(ldHiInvDt)
    lcSelect = " AND InvDate >= ldLwInvDt"
  CASE  EMPTY(ldLwInvDt) .AND. !EMPTY(ldHiInvDt)
    lcSelect = " AND InvDate <= ldHiInvDt"
  CASE !EMPTY(ldLwInvDt) .AND. !EMPTY(ldHiInvDt)
    lcSelect = " AND BETWEEN(InvDate , ldLwInvDt , ldHiInvDt)"               
  OTHERWISE
    lcSelect = ""  
ENDCASE
RETURN(lcSelect)

*!*************************************************************
*! Name      : lfAccSele
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To select the account range and put them in the filter
*:*************************************************************
*! Example     : =lfAccSele()
*!*************************************************************
FUNCTION lfAccSele

*-- Fill the lcSelect variable according to the grid options.
DO CASE
  CASE !EMPTY(lcLwAcc) .AND.  EMPTY(lcHiAcc)
    lcAccSelet = " AND Account >= lcLwAcc"
  CASE  EMPTY(lcLwAcc) .AND. !EMPTY(lcHiAcc)
    lcAccSelet = " AND Account <= lcHiAcc"
  CASE !EMPTY(lcLwAcc) .AND. !EMPTY(lcHiAcc)
    lcAccSelet = " AND BETWEEN(Account , lcLwAcc , lcHiAcc)"
  OTHERWISE
    lcAccSelet = ""  
ENDCASE
RETURN(lcAccSelet)

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To assign the invoice date range to its variables.
*:*************************************************************
*! Example     : =lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
lcObjNam = SYS(18)
ldObjVal = EVALUATE(SYS(18))

IF lcObjNam = "LCOGVALUEF" 
  ldLwInvDt = ldObjVal
ELSE
  ldHiInvDt = ldObjVal
ENDIF
&lcObjNam = ldObjVal

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To validate the account code range.
*:*************************************************************
*! Example     : =lfvAccount()
*!*************************************************************
FUNCTION lfvAccount

lcObjNam = SYS(18)
lcAccNo   = EVALUATE(SYS(18))

IF lcObjNam = "LCOGVALUEF" 
  IF '?' $ lcAccNo .OR. (!EMPTY(lcAccNo) .AND. !SEEK('M' + lcAccNo , 'Customer'))
    =CusBrowM(@lcAccNo , '' , 'M')
  ENDIF
  lcLwAcc = lcAccNo
ELSE
  IF '?' $ lcAccNo .OR. (!EMPTY(lcAccNo) .AND. !SEEK('M' + lcAccNo , 'Customer'))
    =CusBrowM(@lcAccNo , '' , 'M')
  ENDIF
  lcHiAcc = lcAccNo
ENDIF
&lcObjNam = lcAccNo

*!*************************************************************
*! Name      : lfvFactor
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : To validate the factor code.
*:*************************************************************
*! Example     : = lfvFactor()
*!*************************************************************
FUNCTION lfvFactor

IF EMPTY(lcRpFactor)
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Factor code should not be empty.')
ELSE
  =GFVLFLD('SYCFACT','CFACCODE',VARREAD(),'',.F.,.F.,[CFACCODE:H="Factor code",CFACCOMP:H="Factor name",cPhoneNo:H="Phone",cFaxNo:H="Fax"],'1',.F.,.T.)
ENDIF

*!*************************************************************
*! Name      : lfOgWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 10/19/1999
*! Purpose   : Function to be executed in the when of the OG.
*:*************************************************************
*! Example     : = lfOgWhen()
*!*************************************************************
FUNCTION lfOgWhen

R_WIDTH   = 'W'
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)
ENDIF
ldLwInvDt  = gdSysDate
ldHiInvDt  = gdSysDate

IF FILE(gcDataDir+"KRA1400.MEM")
  RESTORE FROM gcDataDir+"KRA1400.MEM" ADDITIVE
ENDIF