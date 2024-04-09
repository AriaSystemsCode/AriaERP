*:***************************************************************************
*: Program file  : ARPINVWW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR WEARWOLF GROUP LTD.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100895)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11"
*: Date          : 05/07/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102327
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVWW
*:***************************************************************************

           *--Section of Initializing the variables --*
llNoRec  = .F.      && Variable used to make the endreport function in the main program.
NEWDOC   = .T.
lnTotQty = 0
lnInvNo  = 2200
STORE 0  TO lnHdrRow,lnLinRow,lnBotRow,MaxRow

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

*--THE COLOR LENGTH
STORE 0 TO lnClrLnWw , lnClrPosWw
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnWw  = LEN(laItemSeg[lnCount,3])
    lnClrPosWw = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  lcInvoice = Invoice

  IF NEWDOC
    lnPage     = 0
    NEWDOC     = .F.
    ldInvDate  = InvHdr.InvDate
    lcOrder    = InvHdr.Order
    lcNote1    = InvHdr.Note1
    lcNote2    = InvHdr.Note2
    lcDept     = InvHdr.Dept
    lcCustPo   = InvHdr.CustPo
    lcAccount  = InvHdr.Account
    lcStore    = InvHdr.Store
    lnCartons  = InvHdr.Cartons
    lnWeight   = InvHdr.Weight
    lcSalesRep = InvHdr.Rep1
    lnFrght    = InvHdr.Freight
    lnDiscount = InvHdr.Discount
    lnTotChrg  = InvHdr.TotalChg
    lcPikTkt   = InvHdr.PikTkt
    lnTotQty   = 0
    lnSubTotal = 0

    *--Get the Descreption about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')

    *-- GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(lcStore = SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    lcBtName = lcSolTName
    lcBtAddr1 = laSoldTo[1]
    lcBtAddr2 = laSoldTo[2]
    lcBtAddr3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       lcBtAddr2 = laSoldTo[3]
       lcBtAddr3 = ''
    ENDIF

    lcStName  = lcShpTName
    lcStAddr1 = laShipTo[1]
    lcStAddr2 = laShipTo[2]
    lcStAddr3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      lcStAddr2 = laShipTo[3]
      lcStAddr3 = ''
    ENDIF
  ENDIF

  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()

  *-------------------
  * START PRINT LINES
  *-------------------
  SELECT INVLINE
  SCAN FOR TotQty <> 0 AND lcInvoice = Invoice
    @ lnLinRow,01 SAY LEFT(STYLE,7)
    @ lnLinRow,09 SAY SCALE.cscl_desc
    @ lnLinRow,20 SAY LEFT(ALLTRIM(Style.Desc),14)
    @ lnLinRow,35 SAY SUBSTR(STYLE,lnClrPosWw,4)
    @ lnLinRow,41 SAY LEFT(gfCodDes(SUBSTR(STYLE,lnClrPosWw,lnClrLnWw),'COLOR'),7)
    @ lnLinRow,51 SAY TotQty PICTURE '999999'
    @ lnLinRow,57 SAY PADL('$'+ALLTRIM(STR(Price,8,2)),9)

    lnExtended = Price*TotQty
    @ lnLinRow,67 SAY PADL('$'+ALLTRIM(STR(lnExtended,10,2)),11)
    lnTotQty = lnTotQty + TotQty
    lnSubTotal = lnSubTotal + lnExtended
    lnLinRow = lnLinRow + 1

    IF lnLinRow >= MaxRow
      @ lnLinRow + 1 , 0 SAY PADC("****** CONTINUED ON NEXT PAGE ******",80," ")
      *--Print the Footer
      =lfPrnFootr()
      =lfGetInHdr()
    ENDIF
  ENDSCAN

  *--Print the Footer
  NEWDOC = .T.
  =lfPrnFootr()

  SELECT INVHDR
  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
    NEWDOC = .T.
  ENDIF

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfPrnFootr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/07/2001
*! Purpose   : To print the invoice footer.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  :
*!*************************************************************
*! Returns            :
*!*************************************************************
*! Example            :  lfPrnFootr()
*!*************************************************************
FUNCTION lfPrnFootr

IF llRpInvLWW
  IF !EMPTY(lcStore)
    @ lnBotRow,01 SAY lcStore+ ' / ' + lcStName
  ENDIF
ENDIF

lcSubTotal = ALLTRIM(STR(lnSubTotal,10,2))

@ lnBotRow,60 SAY "Subt : "+PADL('$'+lcSubTotal,11)
lnBotRow = lnBotRow + 1

IF llRpInvLWW
  @ lnBotRow,01 SAY lcNote1
ENDIF

lcFrght = ALLTRIM(STR(lnFrght,10,2))
@ lnBotRow,60 SAY IIF(NEWDOC,"Frgt : "+ PADL('$'+lcFrght,11),"")
lnBotRow = lnBotRow + 1

IF llRpInvLWW
  @ lnBotRow,01 SAY lcNote2
ENDIF

@ lnBotRow,51 SAY IIF(NEWDOC,ALLTRIM(STR(lnTotQty)),'*****')
lcDiscount = ALLTRIM(STR(lnDiscount,11,2))

lcTotChrg = ALLTRIM(STR(lnTotChrg,10,2))
@ lnBotRow,60 SAY IIF(NEWDOC,IIF(lnDiscount <> 0,"Disc : "+PADL('$'+lcDiscount,11),;
                    "Total: "+PADL('$'+lcTotChrg,11)),"")

lnBotRow = lnBotRow + 1
@ lnBotRow,60 SAY IIF(NEWDOC AND lnDiscount <> 0 ,"Total: "+ ;
                      PADL('$'+lcTotChrg,11),"")

*--End of lfPrnFootr.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 05/07/2001
*! Purpose   : PRINT THE HEADER OF THE INVOICE.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

SELECT INVHDR
lnPage = lnPage + 1

IF lnInvNo = 2200
  lnHdrRow = 05                   && Start header row
  lnLinRow = 22                   && Start lines printing row.
  lnBotRow = 43                   && Start footer row.
  MaxRow   = lnBotRow - 5         && Max row number for lines printing.
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 10        && Start header row.
  lnLinRow = lnHdrRow + 17        && Start lines printing row.
  lnBotRow = lnLinRow + 21        && Start footer row.
  MaxRow   = lnBotRow - 05        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF

*-- Print the Invoice header
@ lnHdrRow,59 SAY ldInvDate
@ lnHdrRow,71 SAY lcInvoice
@ lnHdrRow+3,71 SAY "Page# " + ALLTRIM(STR(lnPage,2))

lnHdrRow = lnHdrRow + 5
lnHdrRow = lnHdrRow + 1

@ lnHdrRow,03 SAY lcBtName
@ lnHdrRow,48 SAY lcStName

lnHdrRow = lnHdrRow + 1

@ lnHdrRow,03 SAY lcBtAddr1
@ lnHdrRow,48 SAY lcStAddr1

lnHdrRow = lnHdrRow + 1

@ lnHdrRow,03 SAY lcBtAddr2
@ lnHdrRow,48 SAY lcStAddr2

lnHdrRow = lnHdrRow + 1

@ lnHdrRow,03 SAY lcBtAddr3
@ lnHdrRow,48 SAY lcStAddr3

lnHdrRow = lnHdrRow + 4

@ lnHdrRow,02 SAY LEFT(lcCustPo,10)
@ lnHdrRow,13 SAY LEFT(lcStore,5)
@ lnHdrRow,18 SAY lcDept
@ lnHdrRow,24 SAY LEFT(lcTerms,13)
@ lnHdrRow,37 SAY ALLTRIM(STR(lnCartons))
@ lnHdrRow,41 SAY ALLTRIM(STR(lnWeight))
@ lnHdrRow,46 SAY LEFT(lcShipVia,11)
@ lnHdrRow,57 SAY lcSalesRep
@ lnHdrRow,61 SAY lcAccount
@ lnHdrRow,66 SAY lcOrder
@ lnHdrRow,72 SAY IIF(!EMPTY(lcPikTkt),lcPikTkt,'')

*--End of lfGetInHdr.