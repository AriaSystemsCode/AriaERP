***************************************************************************
* PROG: ARPINRT.PRG (Custom invoice form for Regent)
* DATE: 10/20/01
* DESC: PRINT INVOICES
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Refer to  : (C102372)
***************************************************************************
*: Calls : 
*:         FUNCTION  : lfGetInHdr()
*:                   : lfPrntHdr()
*:                   : gfModalGen()
*:                   : gfItemMask()
*:                   : gfGetAdr()
*:                   : gfCodDes()
***************************************************************************
*B804567,1 SSE 12/31/2001 Fix bug of making some alignment.
*B605371,1 SSE 01/13/2001 Fix bug of leaving blank lines between pages.

*B605371,1 Initialize the variables used in printing pages. [Begin]
STORE 0 TO lnHdrRow , lnBotRow , MaxRow
lnInvNo = 22
*B605371,1 Initialize the variables used in printing pages. [End]

*--If No records selected.
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*--Open Files
=lfOpenFile()

*-- Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
*--Get the color
STORE 0 TO lnColorLen,lnNonMajSt,ROW
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laAddress[1,1]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*--Set the order into the CODES file.
*-- Bill to AND *- Ship to Addresses.
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcSpcInst
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5
STORE ' ' TO lcInvoice,lcPhone,lcNote1,lcNote2,lcAccount,lcStore,lcTerVCode,lcShiVCode,laAddress
STORE 0.00 TO lnLineTot,lnPieces,lnSubTotal,lnPageNo,lnNoOfPage
STORE 0 TO lnPageNo,lnNoOfPage
llAltShpTo = .F.
*-- MAIN LOOP
SELECT INVHDR
llFirst = .T.
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
lcCurrScl = ' '
*--- We will make SCAN statement within a DO WHILE statement to care the line up.
SCAN WHILE INKEY() <> 32 FOR &lcRpExp
  WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
  *--Get No of pages for the current invoice
  =lfGetPages()
  *-----------------------------------------------------------------
  * Get and print invoice header.
  * Initialize document totals.
  *-----------------------------------------------------------------
  *-- Function to get invoice HEADER information.
  =lfGetInHdr()
  *-- Function to print invoice HEADER information.   
  =lfPrntHdr()
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  
  *B605371,1 Reset the current scale for every new invoice. [Begin]
  lcCurrScl = SPACE(03)
  *B605371,1 Reset the current scale for every new invoice. [End]
  
  SELECT INVLINE
  SCAN WHILE invoice+STR(lineno,6) = lcInvoice
    DO CASE
    
      *B605371,1 Using the varibles used for printing bottom lines. [Begin]
      *CASE ROW >= 39
      *  @ 44,54 SAY 'SEE LAST PAGE ...'
      *  @ 46,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
      CASE ROW >= MaxRow
        lnBotRow = lnBotRow + 5
        @ lnBotRow,54 SAY 'SEE LAST PAGE ...'
        lnBotRow = lnBotRow + 2
        @ lnBotRow,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
      *B605371,1 Using the varibles used for printing bottom lines. [End]

        =lfPrntHdr()
      CASE TOTQTY = 0
        LOOP
      CASE EOF()
        EXIT
    ENDCASE
    *--Get the color desc.
    lcColDesc = SUBSTR(gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen),'COLOR'),1,14)
    
    *B804567,1 Print 14 characters from style description. [Begin]
    *@ ROW,02 SAY Style.Desc
    @ ROW,00 SAY SUBSTR(Style.Desc,1,15)
    *B804567,1 Print 14 characters from style description. [End]
    
    IF SCALE <> lcCurrScl AND SEEK('S'+SCALE,'SCALE')
      lcCurrScl = SCALE
     
      *@ Row,29 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ1,1,3)), 3, ' ')
      *@ Row,33 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ2,1,3)), 3, ' ')
      *@ Row,37 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ3,1,3)), 3, ' ')
      *@ Row,41 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ4,1,3)), 3, ' ')
      *@ Row,45 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ5,1,3)), 3, ' ')
      *@ Row,49 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ6,1,3)), 3, ' ')
      *@ Row,53 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ7,1,3)), 3, ' ')
      *@ Row,57 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ8,1,3)), 3, ' ')
      **
     
      *B804567,1 Increase the size scale to be 5 characters. [Begin]
      *@ Row,26 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ1,1,3)), 3, ' ')
      *@ Row,30 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ2,1,3)), 3, ' ')
      *@ Row,34 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ3,1,3)), 3, ' ')
      *@ Row,38 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ4,1,3)), 3, ' ')
      *@ Row,42 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ5,1,3)), 3, ' ')
      *@ Row,46 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ6,1,3)), 3, ' ')
      *@ Row,50 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ7,1,3)), 3, ' ')
      *@ Row,54 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ8,1,3)), 3, ' ')

      *B605371,1 Add underscore to scale. [Begin]
      *@ Row,16 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ1,1,5)), 5, ' ')
      *@ Row,23 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ2,1,5)), 5, ' ')
      *@ Row,29 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ3,1,5)), 5, ' ')
      *@ Row,34 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ4,1,5)), 5, ' ')
      *@ Row,39 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ5,1,5)), 5, ' ')
      *@ Row,44 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ6,1,5)), 5, ' ')
      *@ Row,49 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ7,1,5)), 5, ' ')
      *@ Row,54 SAY PADL(ALLTRIM(SUBSTR(SCALE.SZ8,1,5)), 5, ' ')

      @ Row,16 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz1,1,5)), 5, IIF(EMPTY(Scale.Sz1)," ","_"))
      @ Row,23 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz2,1,5)), 5, IIF(EMPTY(Scale.Sz2)," ","_"))
      @ Row,29 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz3,1,5)), 5, IIF(EMPTY(Scale.Sz3)," ","_"))
      @ Row,34 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz4,1,5)), 5, IIF(EMPTY(Scale.Sz4)," ","_"))
      @ Row,39 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz5,1,5)), 5, IIF(EMPTY(Scale.Sz5)," ","_"))
      @ Row,44 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz6,1,5)), 5, IIF(EMPTY(Scale.Sz6)," ","_"))
      @ Row,49 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz7,1,5)), 5, IIF(EMPTY(Scale.Sz7)," ","_"))
      @ Row,54 SAY PADL(ALLTRIM(SUBSTR(Scale.Sz8,1,5)), 5, IIF(EMPTY(Scale.Sz8)," ","_"))
      *B605371,1 Add underscore to scale. [End]

      *B804567,1 Increase the size scale to be 5 characters. [End]
      
    ENDIF
    ROW = ROW +1 
    
    *B804567,1 Print 12 character from style and 7 from color description. [Begin]
    *@ ROW,02 SAY SUBSTR(Style,1,lnMajLen)
    *@ ROW,14 SAY lcColDesc
    *@ ROW,14 SAY SUBSTR(lcColDesc,1,10)
    @ ROW,00 SAY SUBSTR(Style,1,lnMajLen-2)
    @ ROW,10 SAY SUBSTR(lcColDesc,1,7)
    *B804567,1 Print 12 character from style and 7 from color description. [End]
    
    *@ ROW,29 SAY QTY1   PICTURE '@Z 999'
    *@ ROW,33 SAY QTY2   PICTURE '@Z 999'
    *@ ROW,37 SAY QTY3   PICTURE '@Z 999'
    *@ ROW,41 SAY QTY4   PICTURE '@Z 999'
    *@ ROW,45 SAY QTY5   PICTURE '@Z 999'
    *@ ROW,49 SAY QTY6   PICTURE '@Z 999'
    *@ ROW,53 SAY QTY7   PICTURE '@Z 999'
    *@ ROW,57 SAY QTY8   PICTURE '@Z 999'
    *@ ROW,61 SAY TOTQTY PICTURE '99999'
    
    *B804567,1 Increase the size scale to be 5 characters. [Begin]
    *@ ROW,26 SAY QTY1   PICTURE '@Z 999'
    *@ ROW,30 SAY QTY2   PICTURE '@Z 999'
    *@ ROW,34 SAY QTY3   PICTURE '@Z 999'
    *@ ROW,38 SAY QTY4   PICTURE '@Z 999'
    *@ ROW,42 SAY QTY5   PICTURE '@Z 999'
    *@ ROW,46 SAY QTY6   PICTURE '@Z 999'
    *@ ROW,50 SAY QTY7   PICTURE '@Z 999'
    *@ ROW,54 SAY QTY8   PICTURE '@Z 999'
    *@ ROW,58 SAY TOTQTY PICTURE '99999'

    @ ROW,15 SAY QTY1   PICTURE '@Z 99999'
    @ ROW,23 SAY QTY2   PICTURE '@Z 99999'
    @ ROW,29 SAY QTY3   PICTURE '@Z 99999'
    @ ROW,34 SAY QTY4   PICTURE '@Z 99999'
    @ ROW,39 SAY QTY5   PICTURE '@Z 99999'
    @ ROW,44 SAY QTY6   PICTURE '@Z 99999'
    @ ROW,49 SAY QTY7   PICTURE '@Z 99999'
    @ ROW,54 SAY QTY8   PICTURE '@Z 99999'
    @ ROW,59 SAY TOTQTY PICTURE '99999'
    *B804567,1 Increase the size scale to be 5 characters. [End]

    *-- CUM INVOICE TOTALS
    lnLineTot = PRICE * TOTQTY
    lnPieces  = lnPieces  + TOTQTY
    lnSubTotal= lnSubTotal+ lnLineTot

    *SSE
    *@ ROW,65 SAY PRICE      PICTURE '9999.99'    
    *@ ROW,74 SAY lnLineTot  PICTURE '999999.99'
    @ ROW,64 SAY PRICE      PICTURE '9999.99'      
    @ ROW,71 SAY lnLineTot  PICTURE '999999.99'
    *SSE
    
    ROW = ROW+1
  ENDSCAN
  *-----------------*  
  * Print the total *
  *-----------------*
  SELECT INVHDR
  ROW = ROW+1  
  
  *B605371,1 Using the varibles used for printing bottom lines. [Begin]
  *IF ROW >= 39
  *  @ 44,54 SAY 'SEE LAST PAGE ...'
  *  @ 46,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
  
  IF ROW >= MaxRow
    lnBotRow = lnBotRow + 5
    @ lnBotRow,54 SAY 'SEE LAST PAGE ...'
    lnBotRow = lnBotRow + 2
    @ lnBotRow,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
  *B605371,1 Using the varibles used for printing bottom lines. [End]
    
    =lfPrntHdr()
  ENDIF  
  
  @ ROW,45 SAY 'Subtotal   :'
  @ ROW,67 SAY lnSubTotal PICTURE '9999999999.99'
  ROW = ROW + 1
  @ ROW,02 SAY lcSpcInst
  @ ROW,45 SAY "Sales Tax  :"
  IF TAX_RATE > 0
    XSTRING_RATE = ALLTRIM(STR (TAX_RATE,5,2))
    @ ROW ,58 SAY XSTRING_RATE + ' %'
    @ ROW ,67 SAY Tax_Amt PICTURE '9999999999.99'
  ENDIF
  ROW = ROW + 1
  @ ROW,45 SAY "Total Units:"
  @ ROW,74 SAY lnPieces PICTURE '999999'
  IF llRpInvNot
    ROW  = ROW + 1
    =lfPrnNotPd()
  ENDIF

  *B804567,1 Shift 1 character to the left. [Begin]
  *@ 44,54 SAY lnSubTotal + Tax_Amt PICTURE '999999.99'
  *@ 44,64 SAY FREIGHT  PICTURE '999999.99'
  *@ 44,74 SAY TOTALCHG PICTURE '999999.99'
  
  *B605371,1 Using the varibles used for printing bottom lines. [Begin]
  *@ 44,53 SAY lnSubTotal + Tax_Amt PICTURE '999999.99'
  *@ 44,62 SAY FREIGHT  PICTURE '999999.99'
  *@ 44,71 SAY TOTALCHG PICTURE '999999.99'
  IF Row < MaxRow
    Row = MaxRow
  ENDIF
  lnBotRow = lnBotRow + 5
  @ lnBotRow,53 SAY lnSubTotal + Tax_Amt PICTURE '999999.99'
  @ lnBotRow,62 SAY FREIGHT  PICTURE '999999.99'
  @ lnBotRow,71 SAY TOTALCHG PICTURE '999999.99'
  lnBotRow = lnBotRow + 2
  *B605371,1 Using the varibles used for printing bottom lines. [End]
  
  *B804567,1 Shift 1 character to the left. [End]
  
  *B605371,1 Using the varibles used for printing bottom lines. [Begin]
  *@ 46,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
  @ lnBotRow,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
  *B605371,1 Using the varibles used for printing bottom lines. [End]
    
  *B804567,1 Add 3 empty lines. [Begin]
  *B605371,1 Commented out. [Begin]
  *@ 47,01 SAY ''
  *@ 48,01 SAY ''
  *@ 49,01 SAY ''
  *B605371,1 Commented out. [End]
  *B804567,1 Add 3 empty lines. [End]
   
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
ENDSCAN 
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN     

*!*************************************************************
* Name      : lfGetInHdr
* Developer : Adel Mohammed El Gazzar (ADEL)
* DATE      : 04/18/1999
* AUTH      : Adel Mohammed El Gazzar (ADEL)
* Purpose   : TO get the information of the invoice header.
* Refer to  : (C101471)
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,laAddress 
STORE 0.00 TO lnPieces, lnSubTot
lcInvoice  = Invoice
lcPhone    = IIF(EMPTY(PHONE),'',PHONE)
lcNote1    = IIF(NOTE1<>'*', NOTE1, '')
lcNote2    = IIF(NOTE2<>'*', NOTE2, '')
lcAccount  = ACCOUNT
lcStore    = STORE
*-- GET THE BILL TO AND SHIP ADDRESS
lcBtName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess.
FOR lnCount = 1 TO ALEN(laAddress,1)-1
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
IF OrdHdr.Alt_ShpTo
  lcStName  = OrdHdr.STNAME
  =gfGetAdr('ORDHDR','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ELSE
  lcStName  = IIF(EMPTY(Customer.DBA),Customer.STNAME,Customer.DBA)
  =gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF
*--Get the term desc.
*--Terms
lcTerVCode =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*-- GET THE DESCRIPTION ABOUT THE CODES
lcShiVCode = SUBSTR(gfCodDes(ShipVia ,PADR('SHIPVIA',10)),1,10)
*--Special instruction
lcSpcInst = gfCodDes(INVHDR.SPCINST , 'SPCINST')
*--Do not get the [FACTOR] NAME & ADDRESS as the report does nor print it.
STORE 0.00 TO lnLineTot,lnPieces,lnSubTotal,lnPageNo
SELECT (lcAlias)

*!*************************************************************
* Name      : lfPrntHdr
* Developer : Adel Mohammed El Gazzar (ADEL)
* DATE      : 04/18/1999
* AUTH      : Adel Mohammed El Gazzar (ADEL)
* Purpose   : TO print the invoice header.
* Refer to  : (C101471)
*!*************************************************************
FUNCTION lfPrntHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT INVHDR

*B605371,1 Reset the invoice variables every special no of invoices. [Begin]
IF lnInvNo = 22
  lnHdrRow = 08
  Row = 23
  lnBotRow = 39
  MaxRow   = lnBotRow
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 13
  Row = lnHdrRow + 15
  lnBotRow = Row + 16
  MaxRow   = lnBotRow
  lnInvNo  = lnInvNo  + 01
ENDIF
*B605371,1 Reset the invoice variables every special no of invoices. [End]

*B605371,1 Printing the Header invoice. [Begin]
*@ 08,08 SAY lcBtName
*@ 08,04 SAY ALLTRIM(lcBtName)
*@ 08,52 SAY lcStName
*@ 08,48 SAY ALLTRIM(lcStName)
*@ 09,08 SAY lcBtAdd1
*@ 09,04 SAY ALLTRIM(lcBtAdd1)
*@ 09,52 SAY lcStAdd1
*@ 09,48 SAY ALLTRIM(lcStAdd1)
*@ 10,08 SAY lcBtAdd2
*@ 10,04 SAY ALLTRIM(lcBtAdd2)
*@ 10,52 SAY lcStAdd2
*@ 10,48 SAY ALLTRIM(lcStAdd2)
*@ 11,08 SAY lcBtAdd3
*@ 11,04 SAY ALLTRIM(lcBtAdd3)
*@ 11,52 SAY lcStAdd3
*@ 11,48 SAY ALLTRIM(lcStAdd3)
*@ 14,40 SAY 'INVOICE# '+lcInvoice

@ lnHdrRow,04 SAY ALLTRIM(lcBtName)    && Line 8
@ lnHdrRow,48 SAY ALLTRIM(lcStName)    
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,04 SAY ALLTRIM(lcBtAdd1)    && Line 9
@ lnHdrRow,48 SAY ALLTRIM(lcStAdd1)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,04 SAY ALLTRIM(lcBtAdd2)    && Line 10
@ lnHdrRow,48 SAY ALLTRIM(lcStAdd2)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,04 SAY ALLTRIM(lcBtAdd3)    && Line 11
@ lnHdrRow,48 SAY ALLTRIM(lcStAdd3)
lnHdrRow = lnHdrRow + 3
@ lnHdrRow,40 SAY 'INVOICE# '+lcInvoice  && Line 14
lnHdrRow = lnHdrRow + 3
*B605371,1 Printing the Header invoice. [End]

*B804567,1 Shift invoice date 2 characters to the left. [Begin]
*@ 17,04 SAY InvDate

*B605371,1 Printin the Header invoice. [Begin]
@ lnHdrRow,02 SAY InvDate                       && Line 17
*B605371,1 Printin the Header invoice. [End]

*B804567,1 Shift invoice date 2 characters to the left. [End]

*B605371,1 Printin the Header invoice. [Begin]
*@ 17,13 SAY lcTerVCode
*@ 17,28 SAY CUSTPO
@ lnHdrRow,13 SAY lcTerVCode
@ lnHdrRow,28 SAY CUSTPO
*B605371,1 Printin the Header invoice. [End]

*B804567,1 Shift invoice date 1 characters to the left. [Begin]
*@ 17,44 SAY PIKTKT
*@ 17,51 SAY lcShiVCode
*@ 17,62 SAY OrdHdr.Order

*B605371,1 Printin the Header invoice. [Begin]
*@ 17,43 SAY PIKTKT
*@ 17,50 SAY lcShiVCode
*@ 17,61 SAY OrdHdr.Order
@ lnHdrRow,43 SAY PIKTKT
@ lnHdrRow,50 SAY lcShiVCode
@ lnHdrRow,61 SAY OrdHdr.Order
*B605371,1 Printin the Header invoice. [End]

*B804567,1 Shift invoice date 1 characters to the left. [End]

*B605371,1 Printin the Header invoice. [Begin]
*@ 17,72 SAY InvHdr.Account
*@ 20,02 SAY IIF(SEEK('M'+INVHDR.ACCOUNT,'CUSTOMER'),Customer.cCusVend,'')
@ lnHdrRow,72 SAY InvHdr.Account       && Line 20
lnHdrRow = lnHdrRow + 3
@ lnHdrRow,02 SAY IIF(SEEK('M'+INVHDR.ACCOUNT,'CUSTOMER'),Customer.cCusVend,'')
*B605371,1 Printin the Header invoice. [End]

lcRep1     = Rep1
*lcSalesNam = IIF(SEEK(lcRep1,"SalesRep"),SalesRep.Name,SPACE(25))
lcSalesNam = IIF(SEEK(lcRep1,"SalesRep"),SalesRep.Name,'')

*B605371,1 Printin the Header invoice. [Begin]
*@ 20,18 SAY lcSalesNam
*@ 20,51 SAY dueDate
*ROW = 23
*@ lnHdrRow,18 SAY lcSalesNam
*@ lnHdrRow,51 SAY dueDate
@ lnHdrRow,18 SAY SUBSTR(lcSalesNam,1,17)
@ lnHdrRow,37 SAY "Invoice Due Date: " + DTOC(dueDate)
lnHdrRow = lnHdrRow + 2
*B605371,1 Printin the Header invoice. [End]

lnPageNo = lnPageNo + 1
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/01
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
llQuit    = .T.
SET MEMOWIDTH TO 66
SELECT NotePad
IF SEEK ('C' + INVHDR.INVOICE)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins

    *B605371,1 Printin the Header invoice. [Begin]
    *IF ROW >= 39
    *  @ 44,54 SAY 'SEE LAST PAGE ...'
    *  @ 46,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)

    IF ROW >= MaxRow
      lnBotRow = lnBotRow + 5
      @ lnBotRow,54 SAY 'SEE LAST PAGE ...'
      lnBotRow = lnBotRow + 2
      @ lnBotRow,20 SAY 'PAGE:' + STR(lnPageNo,3)  +' OF '+ STR(lnNoOfPage,3)
    *B605371,1 Printin the Header invoice. [End]

      =lfPrntHdr()
    ENDIF
    IF !EMPTY(MLINE(MNOTES,lnNotLine)) .AND. ;
         (UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) <>'P';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01))
      IF  SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01)
        @ ROW,02 SAY IIF(UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) = 'I' .AND.;
                         UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1)) = SPACE(01) ,;
                         SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,LEN(ALLTRIM(MLINE(MNOTES,lnNotLine)))), ALLTRIM(MLINE(MNOTES,lnNotLine)))
        ROW  = ROW + 1
      ENDIF
    ENDIF   
    lnNotLine = lnNotLine + 1
  ENDDO     
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)


*---C O U N T   P A G E S
*!*************************************************************
*! Name      : lfGetPages
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/01
*! Purpose   : Calculate No of pages for the current invoice.
*!*************************************************************
*! Example            :  lfGetPages()
*!*************************************************************
FUNCTION lfGetPages

PRIVATE lnAlias,lcKey
lnNoOfPage = 0
lnAlias = ALIAS()
lcKey   = EVAL(KEY())
lnNoOfPage = lnNoOfPage + 1
ROW = 23
lcInvoice  = Invoice
*---------------------------------------------------------------
* [1] LINE ITEM PRINT LOOP
*---------------------------------------------------------------
SELECT INVLINE
SCAN WHILE invoice+STR(lineno,6) = lcInvoice
  DO CASE
    CASE ROW >= 39
      ROW = 23
      lnNoOfPage = lnNoOfPage + 1        
      lcInvoice  = Invoice
    CASE TOTQTY = 0
      LOOP
    CASE EOF()
      EXIT
  ENDCASE
  ROW = ROW +1 
  ROW = ROW+1
ENDSCAN
*-----------------*  
* Print the total *
*-----------------*
SELECT INVHDR
ROW = ROW+1 

IF ROW >= 39
  ROW = 23
  lnNoOfPage = lnNoOfPage + 1  
  lcInvoice  = Invoice
ENDIF
ROW = ROW + 1
ROW = ROW + 1
IF llRpInvNot
  ROW  = ROW + 1
  lnOldMemW = SET("MEMOWIDTH")
  lnNotLine = 1
  llQuit    = .T.
  SET MEMOWIDTH TO 66
  SELECT NotePad
  IF SEEK ('C' + InvHdr.Invoice)
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    DO WHILE lnNotLine <= lnMemLins
      IF ROW >= 39
        ROW = 23
        lnNoOfPage = lnNoOfPage + 1
      
      ENDIF
      IF !EMPTY(MLINE(MNOTES,lnNotLine)) .AND. ;
           (UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) <>'P';
           .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01))
        IF  SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*';
           .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01)
          ROW  = ROW + 1
        ENDIF
      ENDIF   
      lnNotLine = lnNotLine + 1
    ENDDO     
  ENDIF
  SET MEMOWIDTH TO lnOldMemW
ENDIF
SELECT(lnAlias)
=SEEK(lcKey)

FUNCTION lfOpenFile

IF !USED('SALESREP')
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'Salesrep','SH')
ENDIF  
IF !USED('NOTEPAD')
  =gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
ENDIF
