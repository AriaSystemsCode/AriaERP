*:************************************************************************
*: Program file  : ARASSHT.PRG 
*: Program desc. : ASSIGNMENT SHEET REPORT
*: For Report    : (@x,y SAY)
*:         System: ARIA APPAREL SYSTEM 2.7
*: Module        : ACCOUNT RECEIVABLE (AR)
*:      Developer: IHB
*:************************************************************************
*: Calls         : 
*:    Procedures : lpGrid, lpFormS, lpFormL.
*:    Functions  : lfwRunGrid, lfvInvNo, lfwOldVal, lfEvalVars, lfvForm
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example       : DO ARASSHT
*:************************************************************************
*: This Report Program is due to E301092,1
*:B602762,1 IHB 04/18/1999 Fix Default Setting For Currency
*:B802537,1 BWA 10/04/1999 Fix "Amount" column that doesn't print under its 
*:B802537,1 BWA            title,I move the field to the seconed line to be
*:B802537,1 BWA            up with its title.
*:************************************************************************
*-- Option Grid Layout :
*-- Print Form           : (S)hort / (L)ong
*-- Invoice Date         : .. to ..
*-- Restart At Invoice # : greater than or equal to invoice # (INVHDR.INVOICE)
*-- Factor Code          : ..      (Browse from SYCFACT)
*-- Currency             : ..      (select currency from array filled from SYCCURR)
*-- if selected (L)ong Print Form, we add the following to the option grid :
*-- Assignment Sheet  #  : ..   (space (4)) 
*-- Factor Commission %  : ..   (99.9)
*-- Reserve With Held    : ..   (99)
*-- Other Charges        : ..   (999)

*-- xFactor   : variable stores the selected factor.
*-- LDate     : variable stores the selected low date. 
*-- HDate     : variable stores the selected high date. 
*-- LInvoice  : variable stores the selected invoice number.
*-- lcAssSht  : variable stores the entered assignment sheet #.
*-- lnFactCom : variable stores the entered factor commission % .
*-- lnReserPs : variable stores the entered reserve with held .
*-- lnOterChg : variable stores the entered other charges .
*-- lcTot     : used to print amounts with curreny symbol.
xFactor   = laOGFxFlt[3,6]        
LInvoice  = laOGFxFlt[2,6]
lcAssSht  = lcRpAssSht
lnFactCom = lnRpFactCm 
lnReserPs = lnRpReserP
lnOterChg = lnRpOther
lcTot     = ' '          

*-- For bill to & ship to flds. 
DECLARE laAddress[6,3]      && only the first five addresses are needed
STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5

*-- data selection
DO lpGrid

*-- if no records are selected.
IF EOF()
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF  && end if no records are selected

*-- xFName         : factor name
*-- laOGFxFlt[3,6] : stores the selected factor 
xFName = IIF(SEEK(laOGFxFlt[3,6],'SYCFACT'),SYCFACT.CFACCOMP,' ')
*-- lcRpType : stores the selected print form type (either (S)hort or (L)ong) 
*-- if selected to print short form
IF lcRpType = 'S'
  xTitle  = xFName+' ASSIGNMENT NO.________'
ELSE  && else selected long form
  xTitle  = xFName+' ASSIGNMENT NO. '+lcAssSht
ENDIF  && end if selected to print short form
*-- R_TITLE : report tilte 
*-- R_WIDTH : print width 
R_TITLE = 'ASSIGNMENT SCHEDULE'
R_WIDTH = IIF(lcRpType = 'S','N','W')
Row     = 99                 && row number
PageNo  = 0                  && page number 
*-- print either short or long [start] 
DO lpForm&lcRpType
*-- print either short or long [end] 
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN      && return to the caller which is gfopgrid (option grid program)
*-- end of report code...
*--------------------- Functions' And Procedures' Section -----
*--------------------------------------------------------------
*!*************************************************************
*! Name : lpGrid
*! Auth : Timour A. K.
*! Date : 09/01/95.
*!*************************************************************
*! Synopsis : Data Selection
*!*************************************************************
PROCEDURE lpGrid
*-- lcrpexp : is the report filter expression came from option grid,
*-- typically :"INVHDR.STATUS<>"V" 
*--             AND BETWEEN(DTOS(INVHDR.INVDATE),ALLTRIM(DTOS({DD/MM/YYYY}))) 
*--             AND INVHDR.INVOICE >= "INV# " 
*--             AND INVHDR.CFACCODE = "FACTOR" 
*--             AND INVHDR.CCURRCODE = "CURRENCY" "   
SELECT InvHdr
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER
SET FILTER TO &lcRpExp
GO TOP
RETURN

*!*************************************************************
*! Name : lpFormS
*! Auth : Timour A. K.
*! Date : 09/01/95.
*!*************************************************************
*! Synopsis : Report format S .
*!*************************************************************
*! Layout :
*!*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*
*!* INVOICE| INVOICE|  NAME OF CUSTOMER / |TERMS    |AMOUNT      | CREDIT APPR. #
*!* DATE   | NUMBER |  ADDRESS            |         |            | FACTOR#
*!*************************************************************
PROCEDURE lpFormS
*-- lnTotal : variable used to accumulate the total (INVHDR.TOTALCHG)
lnTotal    = 0      
SET DEVICE TO PRINT
*-- print major loop
DO WHILE ! EOF() .AND. INKEY() <> 32
  XINVOICE = INVOICE
  *-- if new page / the first page, print the header 
  IF Row >55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'ARASSHT',XTITLE,R_WIDTH
    @ 06,00 SAY ' INVOICE| INVOICE|  NAME OF CUSTOMER / |TERMS    |AMOUNT      | CREDIT APPR. #'
    @ 07,00 SAY ' DATE   | NUMBER |  ADDRESS            |         |            | FACTOR#       '
    @ 08,00 SAY REPLICATE('*',80)
    Row = 10
  
  ENDIF  && end if new page / the first page

  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  
  *-- line 1
  @ Row,01 SAY INVDATE
  @ Row,11 SAY INVOICE
  @ Row,18 SAY SUBSTR(CUSTOMER.BTNAME,1,19)      && btname of main customer account
  *-- XTERMSD : is the termcode description 
  XTERMSD = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
  
  *B802537,1 BWA 10/04/1999 Fix "Amount" column print to its title [START]  
  *@ Row,40 SAY XTERMSD
  @ Row,40 SAY LEFT(XTERMSD,10)
  *B802537,1 BWA 10/04/1999 [END]
  
  @ Row,51 SAY TOTALCHG PICTURE '9999999.99'
  
  @ Row,64 SAY APPROVAL
  Row = Row + 1

  *-- 
  *-- line 2
  STORE '' TO lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
  =gfGetAdr('Customer','','','',1,'2')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount   = STR(laAddress[lnCount,1],1)
    lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR
  @ Row,18 SAY ALLTRIM(lcBillAdd1)      && addresses either of main account or of store 
  *-- XFACTNO : is factor account.
  XFACTNO =IIF(INVHDR.CFACCODE=CUSTOMER.CFACCODE,SUBSTR(CUSTOMER.FACTACCT,1,10),'')
  
  @ Row,64 SAY XFACTNO
  Row = Row + 1
  *-- line 3
  @ Row,18 SAY lcBillAdd2
  Row=Row+1
  *-- line 4
  @ Row,18 SAY lcBillAdd3
  Row = Row + 1
  *-- line 5
  @ Row,18 SAY lcBillAdd4
  Row = Row + 1
  *-- line 6
  @ Row,18 SAY lcBillAdd5
  Row = Row + 1
  
  *-- line 2
  *@ Row,18 SAY ALLTRIM(CUSTOMER.BTADDR1)      && addresses either of main account or of store 
  *-- XFACTNO : is factor account.
  *XFACTNO =IIF(INVHDR.CFACCODE=CUSTOMER.CFACCODE,SUBSTR(CUSTOMER.FACTACCT,1,10),'')
  *@ Row,64 SAY XFACTNO
  *Row = Row + 1
  *-- line 3
  *@ Row,18 SAY CUSTOMER.BTADDR2
  *Row=Row+1
  *-- line 4
  *@ Row,18 SAY CUSTOMER.BTCITY+' '+CUSTOMER.BTSTATE+' '+CUSTOMER.BTZIP
  *Row = Row + 1
  
  @ Row,00 SAY REPLICATE ('.',79)
  lnTotal = lnTotal + INVHDR.TOTALCHG
  *-- retreive the relation between INVHDR and CUSTOMER based on main account
  SELECT INVHDR
  SKIP     
  Row = Row + 1

ENDDO  && end print major loop
*-- print grand total
*-- laOGFxFlt[4,6] : stores the selected currency code.if the company
*-- is not using multi currency, it will be empty.
Row = Row + 1
@ Row,00 SAY REPLICATE('*',80)
Row = Row + 1
@ Row,00 SAY '*** GRAND TOTAL ***'
*-- if there's a currency code, print total with the currency symbol 
IF !EMPTY(laOGFxFlt[4,6])
  lcTot = gfAddCurSm(laOGFxFlt[4,6],lnTotal,2)          
  
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and grand total [START]  
  *@ Row,49 SAY lcTot
  @ Row,53 SAY lcTot
  *B802537,1 BWA 10/04/1999 [END]

ELSE  && else print total 
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and grand total [START]  
  *@ Row,50 SAY lnTotal PICTURE '9999999.99'
  @ Row,52 SAY lnTotal PICTURE '9999999.99'
  *B802537,1 BWA 10/04/1999 [END]

ENDIF  && end if there's a currency code
Row = Row + 1
@ Row,00 SAY REPLICATE('*',80)
Row=Row+1
@ Row,00 SAY ' '
RETURN

*!*************************************************************
*! Name : lpFormL
*! Auth : Timour A. K.
*! Date : 09/01/95.
*!*************************************************************
*! Synopsis : Report format L .
*!*************************************************************
*! Layout :
*!*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*
*! ACCOUNT NAME.......................... INVOICE INV DATE  GROSS AMT TRADE DISC FACTOR FEE    RESERVE OTHER CHRG
*! ±±±±±   ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± ±±±±±±  99/99/99 9999999.99 9999999.99 9999999.99 9999999.99 9999999.99 
*!*....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*
*!*************************************************************
PROCEDURE lpFormL

SELECT InvHdr
*--Print report sorted by account.
SET ORDER TO TAG InvHdrA          && account+invoice
SUM TotalChg TO lnTotInv
GO TOP
*-- lnSTotalX  : subtotal
*-- lnSNet     : net subtotal 
*-- lnGTotalX  : grand total
*-- lnGnet     : net grand total
STORE 0.00 TO lnSTotal1,lnSTotal2,lnSTotal3,lnSTotal4,lnSTotal5,lnSNet
STORE 0.00 TO lnGTotal1,lnGTotal2,lnGTotal3,lnGTotal4,lnGTotal5,lnGnet
lcAccount = Account
SET DEVICE TO PRINT
*-- major print loop
SCAN WHILE INKEY() <> 32
  *-- if new page / the first page, print the header 
  IF Row > 55
    PageNo = PageNo + 1
    DO RPT_HDR WITH 'ARASSHT',xTitle,R_Width
    @ 05,000 SAY ' ACCOUNT NAME................... INVOICE INV DATE  GROSS AMT TRADE DISC FACTOR FEE    RESERVE OTHER CHRG     NET '
    @ 06,000 SAY REPLICATE('*',132)
    Row = 07
  
  ENDIF   && end if new page / the first page

  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  
  *-- if a new account entries or the first, calculating subtotals per account
  IF Account <> lcAccount
    DO lpSubTot
  
  ENDIF  && end if a new account entries or the first
  lnTradeDis = ( (Trde_Disc/100)*TotalChg )
  lnFactFee  = (TotalChg - lnTradeDis)*(lnFactCom/100)
  lnReserve  = (TotalChg - lnTradeDis)*(lnReserPs/100)
  lnOthers   = (TotalChg / lnTotInv ) * lnOterChg
  lnNet      = TotalChg - (lnTradeDis + lnFactFee + lnReserve + lnOthers)
  @ Row,001 SAY Account
  @ Row,009 SAY SUBSTR(Customer.BtName,1,23)
  @ Row,033 SAY Invoice
  @ Row,041 SAY InvDate
  @ Row,050 SAY TotalChg   PICTURE '9999999.99'
  @ Row,061 SAY lnTradeDis PICTURE '9999999.99'
  @ Row,072 SAY lnFactFee  PICTURE '9999999.99'
  @ Row,083 SAY lnReserve  PICTURE '9999999.99'
  @ Row,094 SAY lnOthers   PICTURE '9999999.99'
  @ ROW,105 SAY lnNet      PICTURE '9999999.99'
  Row = Row + 1
  *-- accumulation for Sub totals.
  lnSTotal1 = lnSTotal1 + TotalChg
  lnSTotal2 = lnSTotal2 + lnTradeDis
  lnSTotal3 = lnSTotal3 + lnFactFee
  lnSTotal4 = lnSTotal4 + lnReserve
  lnSTotal5 = lnSTotal5 + lnOthers
  lnSNet    = lnSNet    + lnNet

ENDSCAN  && end major print loop
DO lpSubTot         && print subtotals.
DO lpGrndTot        && print garnd totals.
RETURN
*!*************************************************************
PROCEDURE lpSubTot
*-- print subtotals.
@ Row,000 SAY REPLICATE ('.',123)
Row = Row + 1
@ Row,000 SAY '*** SUBTOTAL : ' + lcAccount + ' ***'

*-- if there's a currency code, print total with the currency symbol 
IF !EMPTY(laOGFxFlt[4,6])
  lcTot = gfAddCurSm(laOGFxFlt[4,6],lnSTotal1,2)          
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and sub total [START]  
  *@ Row,050 SAY lcTot
  @ Row,050 SAY PADL(lcTot,11,' ')
  *B802537,1 BWA 10/04/1999 [END]
  
ELSE  && else print total
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and sub total [START]  
  *@ Row,049 SAY lnSTotal1 PICTURE '9999999.99'
  @ Row,050 SAY lnSTotal1 PICTURE '9999999.99'
  *B802537,1 BWA 10/04/1999 [END]  

ENDIF  && end if there's a currency code
@ Row,061 SAY lnSTotal2 PICTURE '9999999.99'
@ Row,072 SAY lnSTotal3 PICTURE '9999999.99'
@ Row,083 SAY lnSTotal4 PICTURE '9999999.99'
@ Row,094 SAY lnSTotal5 PICTURE '9999999.99'
@ Row,105 SAY lnSNet    PICTURE '9999999.99'
Row = Row + 1
@ Row,000 SAY REPLICATE ('.',123)
Row = Row + 1
lcAccount = Account
*-- calculating the grand totals.
lnGTotal1 = lnGTotal1 + lnSTotal1
lnGTotal2 = lnGTotal2 + lnSTotal2
lnGTotal3 = lnGTotal3 + lnSTotal3
lnGTotal4 = lnGTotal4 + lnSTotal4
lnGTotal5 = lnGTotal5 + lnSTotal5
lnGNet    = lnGNet    + lnSNet 
STORE 0.00 TO lnSTotal1,lnSTotal2,lnSTotal3,lnSTotal4,lnSTotal5,lnSNet
RETURN

*!*************************************************************
PROCEDURE lpGrndTot
*-- print grand totals.
Row = Row + 1
@ Row,000 SAY  REPLICATE('*',132)
Row = Row + 1
@ Row,000 SAY '*** GRAND TOTAL ***'

***
*-- if there's a currency code, print total with the currency symbol 
IF !EMPTY(laOGFxFlt[4,6])
  lcTot = gfAddCurSm(laOGFxFlt[4,6],lnGTotal1,2)
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and grand total [START]  
  *@ Row,049 SAY lcTot  
  @ Row,52 SAY lcTot
  *B802537,1 BWA 10/04/1999 [END]
  
ELSE  && else print total
  *B802537,1 BWA 10/04/1999 Fix "Amount" column and grand total [START]  
  *@ Row,049 SAY lnGTotal1 PICTURE '9999999.99'
  @ Row,050 SAY lnGTotal1 PICTURE '9999999.99'
  *B802537,1 BWA 10/04/1999 [END]  

ENDIF  && end if there's a currency code
@ Row,061 SAY lnGTotal2 PICTURE '9999999.99'
@ Row,072 SAY lnGTotal3 PICTURE '9999999.99'
@ Row,083 SAY lnGTotal4 PICTURE '9999999.99'
@ Row,094 SAY lnGTotal5 PICTURE '9999999.99'
@ Row,105 SAY lnGNet    PICTURE '9999999.99'
Row = Row + 1
@ Row,000 SAY  REPLICATE('*',132)
Row = Row+1
@ Row,00 SAY ' '
RETURN

*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : IHB
*! Date      : 11/30/1998
*! Purpose   : valid function when run grid.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid
R_WIDTH = IIF(lcRpType = 'S','N','W')   && Width of report either N',W or XW
*-- Fill date field in OG. [begin]
lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
IF EMPTY(laOGFxFlt[lnDatePos,6])
  laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate)+'|'+DTOC(gdSysDate)

ENDIF
*-- Fill date field in OG. [end  ]
*-- assign default value for the factor code 
laOGFxFlt[3,6]  = SYCFACT.CFACCODE

*-- IHB check if SYCFACT has no records then quit report [start]
IF EMPTY(laOGFxFlt[3,6])
  *WAIT WINDOW "Factor file has no records ..."
  *-- No factors found
  *-- < Ok >
  = gfModalGen('TRM40100B00000')
  llOgTrmnat = .T.
  CLEAR READ
ENDIF
*-- IHB [end]
*-- if company uses multi currancy,
*-- prepare the two arrays which will carry the cuurency values 
*-- and the currency descriptions and also assign default value to be displayed 
*-- if multi currency 
IF llMultCurr
  SET ORDER TO CCURRCODE IN SYCCURR     && To VALIDATE currency code.
  lnCurrPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.CCURRCODE'),1)
  
  *B602762,1 Fix Default Setting For Currency IHB [start]
  *laOGFxFlt[lnCurrPos,6] = gcBaseCurr
  *= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  *-- if Default setting.
  IF lnOGSeting = 1
    laOGFxFlt[lnCurrPos,6] = gcBaseCurr
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnCurrPos)) + ',6]')  && Show get Object .
  ENDIF
  *B602762,1 IHB [end]
    
ENDIF  && end if multi currency 

*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : Validation function for the Invoice number field
*!*************************************************************
*! Called from : Invoice number field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvInvNo
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag
lcObjName  = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "Printed = IIF(PrtFlag = 'P' , 'Yes' , 'No') :R :H= 'Printed' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  lcFile_Ttl = 'Receivable invoices' 
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && else no value is returned
    &lcObjName = laOldVal
  ENDIF    && end of if
  SET RELATION OFF INTO CUSTOMER
ENDIF    && end of if
*-- if the INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF
*-- if the CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
                                  && this is a global solution for any object type.

*!*************************************************************
*! Name      : lfEvalVars
*! Developer : IHB
*! Date      : 12/15/1998
*! Purpose   : Assign/Fill Default values used in Option Grid .
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : = lfEvalVars()
*!*************************************************************
FUNCTION lfEvalVars
llMultCurr  = gfGetMemVar('llMulCurr')    && .T., if company use multi currency.
*-- if multi currency evaluate currency arrays [Begin]
IF llMultCurr
  DIMENSION laCurrVal[1,1]
  IF !USED('SYCCURR')
    =gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ENDIF
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  SELECT SYCCURR
  SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
ENDIF
*-- if multi currency evaluate currency arrays [End]

*!*************************************************************
*! Name      : lfvForm
*! Developer : IHB
*! Date      : 12/06/1998
*! Purpose   : To validate the Form Type Option.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvSortBy()
*!*************************************************************
FUNCTION lfvForm
*-- CLEAR READ exits the active READ, 
*-- and returns control to the previous READ level if the active READ 
*-- is nested.
CLEAR READ

*!*************************************************************
*! Name      : lfvFactor
*! Developer : IHB
*! Date      : 12/15/1998
*! Purpose   : Validate factor option.
*!*************************************************************
*! Calls     : ARIABROW()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactor()
*!*************************************************************
FUNCTION lfvFactor
PRIVATE lnCurAlias,lcFactor
lnCurAlias = SELECT(0)
SELECT SycFact
SET ORDER TO TAG Cfaccode
*-- laOGFxFlt[3,6] stores the selected factor value
*-- if there's a factor value and it's not exist in the SYCFACT, browse from SYCFACT
IF !EMPTY(laOGFxFlt[3,6]) .AND. !SEEK(laOGFxFlt[3,6],'SycFact')
  lcBrFields     = [cFacCode:H='Factor',cFacComp:H='Name',cFacCont:H='Contact',cPhoneNo :P= gfPhoneTem() :H='Phone']
  SELECT SycFact
  laOGFxFlt[3,6] = IIF(ARIABROW('',"Factors",gnBrFSRow1, gnBrFSCol1,;
                  gnBrFSRow2, gnBrFSCol2,'','','cFacCode','laBrowArr'),;
                  SycFact.cFacCode,SPACE(6))

ENDIF  && end if there's a factor value and it's not exist in the SYCFACT
*-- factor code cannot be empty
IF EMPTY(laOGFxFlt[3,6])
  WAIT WINDOW 'Factor Code Can Not Be Empty! Try Again.'
  RETURN .F.
ENDIF
*--
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvDate
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Date Range.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDate()
*!*************************************************************
FUNCTION lfvDate
*-- date range can not be empty
IF EMPTY(lcOGValueF) OR EMPTY(lcOGValueTo)
  WAIT WINDOW 'Date Range is Mandatory ! Please Retry'
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvFactCom
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Commission.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvFactCom()
*!*************************************************************
FUNCTION lfvFactCom
IF lnRpFactCm >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvReserPs
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Reserve with Held.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvReserPs()
*!*************************************************************
FUNCTION lfvReserPs
IF lnRpReserP >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvOterChg
*! Developer : IHB
*! Date      : 01/01/1999
*! Purpose   : Validate Factor Otehr charges.
*!*************************************************************
*! Calls     : NONE
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOterChg()
*!*************************************************************
FUNCTION lfvOterChg
IF lnRpOther >= 0
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
