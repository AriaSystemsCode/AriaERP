*:***************************************************************************
*: Program file  : ARPINVAN.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR A'NUE LIGNE (ANU100).
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100869)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES
*! Date          : 11/14/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C102021
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAN
*:***************************************************************************
*:Modifications :
*:C037869,1 NNA 03/14/2004 Add new Additional Size To the form to become (P - S - M - L - L1 - L2)
*:***************************************************************************
           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.
NEWDOC     = .T.
MAXROW     = 47

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

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------

  IF NEWDOC
    STORE 0.00 TO lnPieces, lnSubTotal
    NEWDOC    = .F.
    lcOrder   = ORDER
    lcNote1   = IIF(NOTE1<>'*', NOTE1, '')
    lcNote2   = IIF(NOTE2<>'*', NOTE2, '')
    lcPikTkt  = PIKTKT
    lcAccount = ACCOUNT
    lcStore   = STORE

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(lcStore = SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    xAccount = Account
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
      XBTADDR2 = laSoldTo[3]
      XBTADDR3 = ''
    ENDIF
    xStore   = Store
    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *** GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerms    = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia  = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    lcSeasDsc  = gfCodDes(INVHDR.SEASON,'SEASON')

    CURLOOP = '1'
  ENDIF
  ****** END NewDoc

  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT InvLine
  lcStore = Store
  Row    = 25
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
    SELECT InvLine
    IF EOF() .OR. Invoice <> lcInvoice .OR. Row >= MaxRow
      IF ROW >= MAXROW
        =lfGetcont()
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF
    IF TotQty = 0
      SKIP
      LOOP
    ENDIF
    SELECT InvLine
    *C037869,1 NNA 03/14/2004 (Begin) Add new additional Size 
    *@ ROW,00 SAY SUBSTR(Style,1,7)
    *@ ROW,08 SAY SUBSTR(Style,14,3)             
    *@ ROW,12 SAY Style.Desc
    *@ ROW,37 SAY Qty1   PICTURE '@Z 9999'
    *@ ROW,41 SAY Qty2   PICTURE '@Z 9999'
    *@ ROW,45 SAY Qty3   PICTURE '@Z 9999'
    *@ ROW,49 SAY Qty4   PICTURE '@Z 9999'
    *@ ROW,53 SAY Qty5   PICTURE '@Z 9999'

    @ ROW,00 SAY SUBSTR(Style,1,7)
    @ ROW,08 SAY SUBSTR(Style,14,3)             
    @ ROW,12 SAY Style.Desc
    @ ROW,37 SAY Qty1   PICTURE '@Z 999'
    @ ROW,40 SAY Qty2   PICTURE '@Z 999'
    @ ROW,43 SAY Qty3   PICTURE '@Z 999'
    @ ROW,46 SAY Qty4   PICTURE '@Z 999'
    @ ROW,49 SAY Qty5   PICTURE '@Z 999'
    @ ROW,52 SAY Qty6   PICTURE '@Z 999'
    *C037869,1 NNA (End)

    @ ROW,57 SAY TotQty PICTURE '99999'
    * CUM INVOICE TOTALS
    xLineTot    = Price * TotQty
    lnPieces    = lnPieces + TotQty
    lnSubTotal  = lnSubTotal+xLineTot
    @ ROW,64  SAY Price     PICTURE '9999.99'
    @ ROW,73  SAY xLineTot  PICTURE '999999.99'
    ROW = ROW + 1

    * GET NEXT LINE ITEM
    SELECT InvLine
    SKIP
  ENDDO
  *** END LINE PROCESSING
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(InvLine.Invoice = lcInvoice ,'1','0')

  IF ENDPAGE = '1' .AND. !XNOTE_LOOP
    =lfGetcont()
    LOOP
  ENDIF

  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + lcInvoice
      SEEK 'C' + lcInvoice
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + lcInvoice
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT InvHdr
  IF Tax_Amt <> 0 AND Discount <> 0
    IF ROW < 47
      @ 47,64 SAY 'Total'
      @ 47,73 SAY lnSubTotal PICTURE '9999999.99'
      @ 49,64 SAY 'Tax'
      @ 49,73 SAY Tax_Amt
      @ 51,64 SAY 'Discount'
      @ 51,73 SAY Discount   PICTURE '9999999.99'
      lnSubTotal=lnSubTotal+Discount+Tax_Amt
    ELSE
      LOOP
    ENDIF
  ENDIF

  IF Tax_Amt=0 AND Discount <> 0 
    IF ROW < 49
      @ 49,64 SAY 'Total'
      @ 49,73 SAY lnSubTotal PICTURE '9999999.99'
      @ 51,64 SAY 'Discount'
      @ 51,73 SAY Discount   PICTURE '9999999.99'
      lnSubTotal=lnSubTotal+Discount
    ELSE 
      LOOP
    ENDIF
  ENDIF
  
  IF Tax_Amt<> 0 AND Discount = 0 
    IF ROW < 49
      @ 49,64 SAY 'Total'
      @ 49,73 SAY lnSubTotal PICTURE '9999999.99'
      @ 51,64 SAY 'Tax'
      @ 51,73 SAY Tax_Amt
      lnSubTotal=lnSubTotal+Tax_Amt
    ELSE 
      LOOP
    ENDIF
  ENDIF

  lcFreight=Freight + Insur + COD
  @ 52,16 SAY lcNote1
  @ 53,16 SAY lcNote2
  @ 53,56 SAY Ship       PICTURE '999999'
  @ 53,73 SAY lnSubTotal PICTURE '9999999.99' 
  @ 55,73 SAY lcFreight  PICTURE '9999999.99'
  @ 57,73 SAY TotalChg   PICTURE '9999999.99'
  @ 59,04 SAY 'DUE DATE: '
  @ 59,17 SAY DueDate

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

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 05/16/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
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
@ 09,10 SAY xAccount
@ 09,52 SAY xStore
@ 10,10 SAY xBtName
@ 10,52 SAY xStName
@ 11,10 SAY xBtAddr1
@ 11,52 SAY xStAddr1
@ 12,10 SAY xBtAddr2
@ 12,52 SAY xStAddr2
@ 13,10 SAY xBtAddr3
@ 13,52 SAY xStAddr3
@ 19,01 SAY lcInvoice
@ 19,10 SAY InvDate
@ 19,20 SAY LEFT(CUSTPO,10)
@ 19,36 SAY ShipVia
@ 19,43 SAY Rep1
@ 19,47 SAY Rep2
@ 19,52 SAY Order
@ 19,63 SAY lcPikTkt
@ 19,73 SAY LEFT(lcTerms,14)
@ 22,01 SAY lcSeasDsc
@ 22,58 SAY Cartons PICTURE '9999'
@ 22,65 SAY Weight  PICTURE '99999'
@ 22,71 SAY SUBSTR(lcShipVia,1,13)

Row    = 25
*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 08/02/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT InvHdr
@ 52,16 SAY InvHdr.Note1 
@ 53,16 SAY InvHdr.Note2
