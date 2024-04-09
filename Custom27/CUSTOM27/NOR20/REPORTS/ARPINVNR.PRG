*:***************************************************************************
*: Program file  : ARPINVNR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR NORTH PORT APPAREL.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101092)
*: Date          : 02/21/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)	
*: Tracking Job Number: C102207
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVNR
*:***************************************************************************
*:Modifications:
*:B804110,1 BWA 04/04/2001 Some adjustements in the custom invoice form.
*:B804146,1 BWA 17/04/2001 Some adjustements in the custom invoice form.
*:***************************************************************************

*--Open the Salesrep file.
IF !USED(lcSalsRep)
  =gfOpenFile(gcDataDir+"SALESREP","SALESREP", 'SH', @lcSalsRep, .T.)
ENDIF

           *--Section of Initializing the variables --*
llNoRec = .F.      && Variable used to make the endreport function in the main program.
*lnInvNo = 22
lnInvNo = 22 *10
STORE 0 TO lnHdrRow,lnLinRow,lnBotRow,MaxRow
STORE SPACE(5)  TO lnCartons,lcShpVaCod,lnWeight
STORE SPACE(6)  TO lcDept
STORE SPACE(7)  TO lcVDisc,lcIDisc,lcTDisc,lcStore
STORE SPACE(9)  TO lcInvoice
STORE SPACE(10) TO lcAccount
STORE SPACE(15) TO lcCustPo
STORE SPACE(16) TO lcPhone
STORE SPACE(25) TO lcSalesRep
STORE SPACE(30) TO lcFBtName,lcFBtAdd1,lcFBtAdd2,lcFBtAdd3
STORE SPACE(30) TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3
STORE SPACE(30) TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4

*--The color length.
STORE 0 TO lnClrLn , lnClrPos
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLn  = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*-- Main loop.
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  lcInvoice = Invoice
  lnTotAmt  = 0
   = lfGtInInfo()      && Load InvHdr And Search for Store
   = lfSay()           && Say Header Line Detail

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*:*************************************************************
*: Name      : lfGtInInfo
*: Developer : BASSEM RAFAAT (BWA)
*: Date      : 02/20/2001
*: Purpose   : To Load InvHdr Variable And Search for Store.
*:*************************************************************
*: Calls   : ''
*:*************************************************************
*: Passed Parameters  : ''
*:*************************************************************
*: Example :  = lfGtInInfo()
*:*************************************************************
FUNCTION lfGtInInfo

PRIVATE lnAlias
lnAlias = SELECT()

SELECT INVHDR
lcInvoice  = Invoice
lcShpVaCod = ShipVia
lnCartons  = Cartons
lnTotAmt   = TotalChg
lnWeight   = Weight
lcAccount  = Account
lcStore    = Store
lcCustPo   = CustPo
lcDept     = Dept
lcPhone    = PHONE

SELECT CUSTOMER
SEEK IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore)

=lfSolSpAdr()    && function in the main program (ARPINV) to get the adrress.

*--Factor to Address.
lcFBtName = SUBSTR(lcFacName,1,25)
lcFBtAdd1 = laFactor[1]
lcFBtAdd2 = laFactor[2]
lcFBtAdd3 = laFactor[3]
IF LEN(TRIM(laFactor[2])) = 0
  lcFBtAdd2 = laFactor[3]
  lcFBtAdd3 = SPACE(30)
ENDIF

*--Bill to Address.
lcBtName = lcSolTName
lcBtAdd1 = laSoldTo[1]
lcBtAdd2 = laSoldTo[2]
lcBtAdd3 = laSoldTo[3]
IF LEN(TRIM(laSoldTo[2])) = 0
  lcBtAdd2 = laSoldTo[3]
  lcBtAdd3 = SPACE(30)
ENDIF

*-- Ship to Address.
lcStName = lcShpTName
lcStAdd1 = laShipTo[1]
lcStAdd2 = laShipTo[2]
lcStAdd3 = laShipTo[3]
IF LEN(TRIM(laShipTo[2])) = 0
  lcStAdd2 = laShipTo[3]
  lcStAdd3 = SPACE(30)
ENDIF

*--Get the Descreption about the codes.
SELECT CODES
SET ORDER TO CODES IN CODES
lcTDisc = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')

*B804110,1 BWA 04/04/2001 Some adjustements in the custom invoice form.[START]
lcVDisc = LEFT(gfCodDes(INVHDR.SHIPVIA,'SHIPVIA'),20)
*B804110,1 [END]

lcIDisc = gfCodDes(INVHDR.SPCINST,'SPCINST')

*--Get Salesrep name.
lcSalesRep = IIF(SEEK(INVHDR.Rep1,lcSalsRep),&lcSalsRep..Name,SPACE(25))
SELECT(lnAlias)

*--End of lfGtInInfo.
*!*************************************************************
*! Name      : lfSay
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/21/2001
*! Purpose   : To print All Invoice {Header,Line,Footer}.
*!*************************************************************
*! Calls     : = lfSayHdr()        && Say Header Data
*!           : = lfSayLin()        && Say Line Data
*!           : = lfPrnFootr()      && Say Footer
*!*************************************************************
*! Passed Parameters    : ''
*!*************************************************************
*! Returns   : ''
*!*************************************************************
*! Example   : = lfSay()
*!*************************************************************
FUNCTION lfSay

= lfSayHdr()      && Say Header Data
= lfSayLin()      && Say Line Data
= lfPrnFootr()    && Say Footer

*--End of lfSay.
*:*************************************************************
*: Name      : lfSayHdr
*: Date      : 02/21/2001
*: Developer : BASSEM RAFAAT (BWA)
*: Purpose   : To Print Header.
*:*************************************************************
*: Calls     : ""
*:*************************************************************
*: Passed Parameters : ''
*:*************************************************************
*: Example   : = lfSayHdr()
*:*************************************************************
FUNCTION lfSayHdr

PRIVATE lnAlias
lnAlias = SELECT()
*IF lnInvNo = 22
IF lnInvNo = 22*10
  lnHdrRow = 10                   && Start header row
  lnLinRow = 28                   && Start lines printing row.
  lnBotRow = 49                   && Start footer row.
  MaxRow   = lnBotRow - 2         && Max row number for lines printing.
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 12        && Start header row.
  lnLinRow = lnHdrRow + 18        && Start lines printing row.
  lnBotRow = lnLinRow + 21        && Start footer row.
  MaxRow   = lnBotRow - 02        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF

SELECT InvHdr
@lnHdrRow - 5,25 SAY IIF(llPrntComp , IIF(EMPTY(lcDivLName),'',lcDivLName) , SPACE(0))
@lnHdrRow - 4,25 SAY IIF(llPrntComp , HLINE2 , SPACE(0))

IF !EMPTY(HLINE3)
  IF llPrntComp
    @lnHdrRow - 3,25 SAY HLINE3
    @lnHdrRow - 2,25 SAY HLINE4
    @lnHdrRow - 1,25 SAY lcPhone SIZE 1,16
  ENDIF
ELSE
  IF llPrntComp
    @lnHdrRow - 3,25 SAY HLINE4
    @lnHdrRow - 2,25 SAY lcPhone SIZE 1,16
  ENDIF
ENDIF

@ lnHdrRow,06 SAY lcBtname          &&-- Print the Invoice header
@ lnHdrRow,50 SAY lcStname
lnHdrRow = lnHdrRow + 1

@ lnHdrRow,06 SAY lcBtAdd1          &&-- Print the Invoice header
@ lnHdrRow,50 SAY lcStAdd1
lnHdrRow = lnHdrRow + 1

@ lnHdrRow,06 SAY lcBtAdd2
@ lnHdrRow,50 SAY lcStAdd2
lnHdrRow = lnHdrRow + 1

@ lnHdrRow,06 SAY lcBtAdd3
@ lnHdrRow,50 SAY lcStAdd3
lnHdrRow = lnHdrRow + 5
@ lnHdrRow-1,16 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE) , lcFBtname , SPACE(0))
@ lnHdrRow,05 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE) , lcFBtAdd1 + SPACE(1) , SPACE(0))

@ lnHdrRow,43 SAY lcVDisc

@ lnHdrRow,65 SAY lnCartons PICTURE "9999"
@ lnHdrRow,69 SAY lnWeight  PICTURE "99999"
@ lnHdrRow,74 SAY LEFT(lcShpVaCod , 6)
@ lnHdrRow,80 SAY LEFT(lcSalesRep , 4)

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE) , lcFBtAdd2 , SPACE(0))

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY IIF(llPrnFact AND !EMPTY(INVHDR.cFACCODE) ,  lcFBtAdd3 , SPACE(0))

lnHdrRow = lnHdrRow + 1

*B804146,1 BWA 17/04/2001 Some adjustements in the custom invoice form.[START]
*@ lnHdrRow,43 SAY lcIDisc
*@ lnHdrRow,58 SAY lcTDisc

@ lnHdrRow,43 SAY LEFT(lcIDisc,15)
@ lnHdrRow,58 SAY LEFT(lcTDisc,15)
*B804146,1 [END]

lnHdrRow = lnHdrRow + 3

@ lnHdrRow,03 SAY  lcAccount
@ lnHdrRow,14 SAY  LEFT(lcStore,7)
@ lnHdrRow,22 SAY  lcInvoice
@ lnHdrRow,31 SAY  InvHdr.Invdate
@ lnHdrRow,52 SAY  LEFT(lcCustPo,10)
@ lnHdrRow,66 SAY  lcDept
SELECT(lnAlias)

*--End of lfSayHdr.
*:*************************************************************
*: Name      : lfSayLin
*: Date      : 02/21/2001
*: Developer : BASSEM RAFAAT (BWA)
*: Purpose   : To Print Lines of the selected invoices.
*:*************************************************************
*: Calls     : ''
*:*************************************************************
*: Passed Parameters  : ''
*:*************************************************************
*: Example   : = lfSayLin()
*:*************************************************************
FUNCTION lfSayLin

SELECT INVLINE
SCAN FOR TotQty<>0 AND lcInvoice = Invoice
  IF lnLinRow >= MaxRow
    @ lnLinRow + 1 , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
    = lfSayHdr()
 ENDIF
 @ lnLinRow+1,03 SAY LEFT(STYLE,12)
 @ lnLinRow+1,16 SAY SUBSTR(STYLE,lnClrPos,6)
 @ lnLinRow+1,30 SAY Scale
 @ lnLinRow+1,33 SAY Qty1           PICTURE "999"
 @ lnLinRow+1,36 SAY Qty2           PICTURE "999"
 @ lnLinRow+1,39 SAY Qty3           PICTURE "999"
 @ lnLinRow+1,42 SAY Qty4           PICTURE "999"
 @ lnLinRow+1,45 SAY Qty5           PICTURE "999"
 @ lnLinRow+1,48 SAY Qty6           PICTURE "999"
 @ lnLinRow+1,51 SAY Qty7           PICTURE "999"
 @ lnLinRow+1,54 SAY Qty8           PICTURE "999"
 @ lnLinRow+1,62 SAY TotQty         PICTURE '99999'
 @ lnLinRow+1,67 SAY Price          PICTURE '99999.99'
 @ lnLinRow+1,76 SAY (Price*TotQty) PICTURE '99999.99'
 lnLinRow = lnLinRow + 1
 @lnLinRow+1,03 SAY LEFT(Style.Desc,15)
 @ lnLinRow+1,19 SAY LEFT(gfCodDes(SUBSTR(INVLINE.STYLE,lnClrPos,6),'COLOR'),10)
lnLinRow = lnLinRow + 1
ENDSCAN

IF lnLinRow >= MaxRow -4
  @ lnLinRow + 1 , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
  = lfSayHdr()
ENDIF
lnLinRow =lnLinRow + 1
= lfSayTotal()

*--End of lfSayLin.
*!*************************************************************
*! Name      : lfPrnFoot
*: Developer : BASSEM RAFAAT (BWA)
*: Date      : 02/21/2001
*! Purpose   : To print the invoice Footer.
*!*************************************************************
*! Calls     : ''
*!*************************************************************
*! Passed Parameters  : ''
*!*************************************************************
*! Returns   : ''
*!*************************************************************
*! Example   : = lfPrnFootr()
*!*************************************************************
FUNCTION lfPrnFootr

@ lnBotRow+1,03 SAY  lcAccount
@ lnBotRow+1,13 SAY  lcInvoice
@ lnBotRow+1,21 SAY  lcStore
@ lnBotRow+1,30 SAY  lcCustPo
@ lnBotRow+1,45 SAY  lcDept
@ lnBotRow+1,70 SAY  lnTotAmt

*--End of lfPrnFootr.
*:*************************************************************
*: Name      : lfSayTotal
*: Developer : BASSEM RAFAAT (BWA)
*: Date      : 02/21/2001
*: Purpose   : To Say Total(MERCHANDISE,FREIHT,DISCOUNT).
*:*************************************************************
*: Calls     : ''
*:*************************************************************
*: Passed Parameters  : ''
*:*************************************************************
*: Return    : ''
*:*************************************************************
*: Example   : = lfSayTotal()
*:*************************************************************
FUNCTION lfSayTotal

IF SEEK(lcInvoice,'InvHdr')
  @lnLinRow,03 SAY "TOTAL - MERCHANDISE "+SPACE(7)+"TOTAL QTY : "+;
                     ALLTRIM(STR(InvHdr.Ship))+SPACE(16)+"TOTAL AMOUNT : "+ALLTRIM(STR(InvHdr.ShipAmt,10,2))
  lnLinRow  = lnLinRow+1
  lnFreight = InvHdr.Freight+InvHdr.Insur +InvHdr.Cod
  @lnLinRow,03 SAY "TOTAL - FREIGHT"+SPACE(24)+ALLTRIM(STR(lnFreight,12,2))
  lnLinRow  = lnLinRow+1
  @lnLinRow,03 SAY "TOTAL - DISCOUNT"+SPACE(23)+ALLTRIM(STR(InvHdr.Discount,11,2))
ENDIF

*--End of lfSayTotal.