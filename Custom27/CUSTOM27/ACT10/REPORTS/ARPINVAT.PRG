*:***************************************************************************
*: Program file  : ARPINVAT
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR Accent Mercantile (ACT10)
*: 				   The program is a conversion from 2.6 ..ACT10\INV810Z.PRG	
*! Date          : 04/24/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101479
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAT
*:***************************************************************************

           *-- Section of initial the program variables --*
NewDoc     = .T.
MaxRow     = 52
Row        = 00
llEndONot  = .F.
llNoteLoop = .F.  && Flag to indicate whether we have finished printing the Notepad or not.
STORE 1 TO lnNotOLine,lnNotILine
STORE .F. TO llNoRec

*-- End Section of initial the program variables --*

*-- SECTION: MAIN LOOP
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SCAN FOR &lcASExp
  xInvoice = Invoice

  *--Get invoice header information.
  IF NewDoc
    *--Check invoice line.
    SELECT INVLINE
    SEEK xInvoice 
    NewDoc    = .F.
    XORDER    = ORDER
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE

    *--Get the bill to and shio to adresses.
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)    
    
    =lfSolSpAdr()
    XBTNAME = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
    IF LEN(TRIM(laSoldTo[2])) =0
      XBTADDR2 = laSoldTo[3]
      XBTADDR3 = ''
    ENDIF

    XSTNAME = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *--Get the description about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES 
    PTERMS   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')

    STORE 0.00 TO xPieces,xSubTotal

  ENDIF  &&END NEWDOC
  ***

  *--Start Print Header.
  =lfGetInHdr()
  
  *--LINE ITEM PRINT LOOP.
  SELECT INVLINE
  Row    = 27
  DO WHILE .T.
    IF EOF() .OR. Invoice <> xInvoice .OR. Row>=MaxRow
      IF ROW >= MAXROW
        DO lpFooter WITH .T.
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT
    ENDIF
    SELECT INVLINE
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF

    @ ROW,00 SAY LEFT(Style,12)
    lcStyDesc=IIF(SEEK(INVLINE.Style,'STYLE'),STYLE.Desc,'')
    @ ROW,16 SAY lcStyDesc
    @ ROW,57 SAY TotQty           PICTURE '99999'
    @ ROW,63 SAY Price            PICTURE '9999.99'
    @ ROW,71 SAY (Price * TotQty) PICTURE '999999.99'

    *--SUM INVOICE TOTALS.
    XLINETOT   = Price * TotQty
    XPIECES    = XPIECES + TotQty
    XSUBTOTAL  = XSUBTOTAL + XLINETOT

    ROW = ROW + 1
    IF SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
      @ ROW,00 SAY "SKU N# : " +;
            LEFT(IIF(!EMPTY(Spck_Lin.Sku),Spck_Lin.Sku,Spck_Lin.Pack_Id),8)
      ROW = ROW + 1
    ENDIF

    * GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO

  SET DEVICE TO PRINT
  *--CURRENTLY PROCESSING REGULAR LINE ITEMS.
  ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
  llNoteLoop = (InvLine.Invoice <> xInvoice) AND (ROW < MaxRow)
  IF ENDPAGE = '1' .AND. .NOT. llNoteLoop
    DO lpFooter WITH .T.
    LOOP
  ENDIF

  *** Print invoice Notepad,and check if the user wanted to print the notepad or not.
  IF llRpInvNot
    *-- Print Order Notepad.
    IF SEEK('B' + XORDER ,'NOTEPAD')
      DO lpNotePad WITH 'O'
      IF llNoteLoop
        DO lpFooter WITH .T.
        LOOP
      ENDIF
    ENDIF

    *-- Print invoice Notepad.
    IF SEEK('C' + XINVOICE ,'NOTEPAD')
      DO lpNotePad WITH 'I'
      IF llNoteLoop
        DO lpFooter WITH .T.
        LOOP
      ENDIF
    ENDIF
  ENDIF
  DO lpFooter WITH .F.
   
ENDSCAN
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : PROCEDURE lpFooter
*! Developer : BASSEM RAFAAT 
*! Date      : 04/24/2000
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
*! Example     : DO lpFooter
*!*************************************************************
PROCEDURE lpFooter
PARA llContine

SELECT INVHDR
IF llContine
  @ ROW+1,16 SAY 'C O N T I N U E D ...'
  @ 59,71 SAY '******.**'
ELSE

  @ 54,71 SAY XSUBTOTAL PICTURE '999999.99'
  @ 56,71 SAY ( FREIGHT + INSUR + COD - ABS(DISCOUNT) ) PICTURE '999999.99'
  @ 59,71 SAY TOTALCHG  PICTURE '9999999.99'
ENDIF


*!*************************************************************
*! Name      : PROCEDURE lpNotePad
*! Developer : BASSEM RAFAAT 
*! Date      : 04/24/2000
*! Purpose   : Print Order and invoice notepad.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpNotePad
*!*************************************************************
PROCEDURE lpNotePad
PARA lcNType

SELECT NOTEPAD
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
lnMemLins = MEMLINES(NOTEPAD.MNOTES)

llFirst =.T.
lnNotLine=IIF(lcNType='O',lnNotOLine,lnNotILine)

DO WHILE lnNotLine <= lnMemLins
  IF ROW >= MaxRow
    llNoteLoop = .T.
  ELSE
    IF llFirst
      llFirst =.F.
      @ ROW,01 SAY IIF(lcNType='O' ,'*-----ORDER NOTES------*',; 
                                    '*-----INVOICE NOTES------*')
      ROW = ROW + 1 
    ENDIF
    @ ROW,01 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
    ROW = ROW + 1
    llNoteLoop = .F.
  ENDIF    
  IF ROW >= MAXROW
    DO lpFooter WITH .T.
    =lfGetInHdr()
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO

IF lcNType='O'
  lnNotOLine=lnNotLine
ELSE
  lnNotILine=lnNotLine
ENDIF

llNoteLoop = (lnNotLine <= lnMemLins)
SET MEMOWIDTH TO lnOldMemW
IF !llNoteLoop .AND. lnMemLins <> 0  .AND.;
       ( lcNType='I' .OR. (lcNType='O' .AND. !llEndONot) )
  @ ROW,01 SAY IIF(lcNType='O' ,'*--END OF ORDER NOTES--*',;
                                '*--END OF INVOICE NOTES--*')
  llEndONot = .T.
  ROW = ROW + 1 
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 04/24/2000
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
@ 11,62 SAY Invoice
@ 14,05 SAY xBtName
@ 14,44 SAY xStName
@ 15,05 SAY xBtAddr1
@ 15,44 SAY xStAddr1
@ 16,05 SAY xBtAddr2
@ 16,44 SAY xStAddr2
@ 17,05 SAY xBtAddr3
@ 17,44 SAY xStAddr3
@ 22,00 SAY LEFT(CustPo,10)
@ 22,12 SAY Dept
@ 22,18 SAY xStore
@ 22,29 SAY InvDate
@ 22,40 SAY SUBSTR(PSHIPVIA,1,12)
@ 22,53 SAY Account
@ 22,60 SAY SUBSTR(PTERMS,1,11)
@ 22,74 SAY Rep1

Row    = 27
*-- End of lfGetInHdr.