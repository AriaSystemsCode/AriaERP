*:***************************************************************************
*: Program file  : ARPINVTM.PRG
*: Program desc. : Customized Invoice Form for Tamotsu .
*: Date          : 01/15/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Tracking Job Number: C102523,1
*:***************************************************************************
*: Example : DO ARPINVTM
*:***************************************************************************
*
XNOTE_LOOP = .F.  && Flag to indicate whether we have finished printing Notepad or not.
llNoRec = .F.
NEWDOC = .T.
MAXROW = 47
lnNotLine = 1

lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

PRIVATE lnStylePos , lnStyleLen , lnClrPos , laItemSeg
STORE 0 TO lnStylePos , lnClrPos
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'   && Free (Style)
    lnStyleLen = LEN(laItemSeg[lnCount,3])
    lnStylePos = laItemSeg[lnCount,4]
  ENDIF
  
  IF laItemSeg[lnCount,1]='C'   && Color
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

SELECT InvHdr
LOCATE FOR &lcRpExp

SET DEVICE TO PRINT
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SCAN FOR &lcRpExp
  XInvoice = Invoice

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------

  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL

    SELECT InvHdr
    NEWDOC    = .F.
    lcOrder   = Order
    lcPhone   = IIF(EMPTY(PHONE),'',PHONE)
    *XNote1    = IIF(Note1<>'*', Note1, '')
    *XNote2    = IIF(Note2<>'*', Note2, '')
    lcAccount = Account
    lcStore   = Store

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore)
    
    =lfSolSpAdr()
    
    XBTNAME  = lcSolTName
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

    SELECT OrdHdr
    SEEK 'O' + lcOrder

    lcCusPo = SUBSTR(InvHdr.CustPo,1,10)
    lcDept  = Dept
    lcRep1 = Rep1
    lcRep2 = Rep2
    
    lcShipDesc = gfCodDes(OrdHdr.ShipVia,'SHIPVIA')
    PTerms = gfCodDes(InvHdr.cTermCode,'CTERMCODE')

    *-- Find the invoice lines.
    SELECT InvLine
    SEEK XInvoice
    
    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF

  *-----------------
  * START PRINT
  *-----------------
  DO lpPrintHdr
  
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT InvLine
  XStore = Store
  ROW = 23
  
  DO WHILE CURLOOP = '1' AND !XNOTE_LOOP
    SELECT InvLine
    IF EOF() OR Invoice <> XInvoice OR ROW >= MAXROW
      IF ROW >= MAXROW
        DO lpContPage
        DO lpPrintHdr
        LOOP
      ENDIF
    
      EXIT
    ENDIF
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    KEY = InvLine.STYLE
    =SEEK (KEY,'STYLE')
    @ ROW,01 SAY SUBSTR(STYLE,lnStylePos,lnStyleLen)
    @ ROW,15 SAY SUBSTR(Style,lnClrPos,6)
    @ ROW,23 SAY SUBSTR(Style.Scale,1,1)
    @ ROW,25 SAY Qty1   PICTURE '@Z 9999'
    @ ROW,29 SAY Qty2   PICTURE '@Z 9999'
    @ ROW,33 SAY Qty3   PICTURE '@Z 9999'
    @ ROW,37 SAY Qty4   PICTURE '@Z 9999'
    @ ROW,41 SAY Qty5   PICTURE '@Z 9999'
    @ ROW,45 SAY Qty6   PICTURE '@Z 9999'
    @ ROW,49 SAY Qty7   PICTURE '@Z 9999'
    @ ROW,53 SAY Qty8   PICTURE '@Z 9999'
    @ ROW,57 SAY TotQty PICTURE '99999'

    *-- Invoice TOTALS
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    @ ROW,63  SAY PRICE     PICTURE '9999.99'
    @ ROW,72  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW + 1

    @ ROW,01 SAY 'STYLE DESC : ' + Style.Desc
    ROW = ROW + 1
     
    SELECT InvLine
    SKIP
  ENDDO
  *** END LINE PROCESSING

  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(InvLine.Invoice = XInvoice ,'1','0')

  IF ENDPAGE = '1' AND !XNOTE_LOOP
    DO lpContPage
    LOOP
  ENDIF

  IF llRpInvNot
    *-- Print invoice Notepad.
    SELECT Notepad
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    IF TYPE + KEY <> 'C' + XInvoice
      SEEK 'C' + XInvoice
    ENDIF
    lnMemLins = MEMLINES(Notepad.mNotes)
    IF TYPE + KEY = 'C' + XInvoice
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
   
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
          *EXIT
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW      && if there is a line will de delete from 
          DO lpContPage
          DO lpPrintHdr
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO

      IF !XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        @ ROW+1,12 SAY 'C O N T I N U E D ...'
        SELECT InvHdr       
        @ 54,01 SAY lcPhone PICTURE lcPhonPict SIZE 1,16        
        @ 54,18 SAY InvHdr.NOTE1
        @ 55,18 SAY InvHdr.NOTE2     
        IF !EMPTY(lcRpMsg1 + lcRpMsg2)
          @ 56,10 SAY lcRpMsg1
          @ 57,10 SAY lcRpMsg2
        ENDIF
        @ 59,01 SAY InvHdr.PikTkt
        @ 59,09 SAY InvHdr.ShipDate
        @ 59,19 SAY '***'
        @ 59,25 SAY '*****'
        @ 59,71 SAY '******.**'
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT InvHdr
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,54 SAY XPIECES              PICTURE '999999'
  @ ROW,70 SAY XSUBTOTAL            PICTURE '9999999.99'
  ROW=ROW+1

  IF DISCOUNT<>0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,69 SAY DISCOUNT         PICTURE '99999999.99'
    ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF llTax AND lcTaxMeth = 'M' 
    @ ROW,12 SAY lcTaxDesc
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY STR(InvHdr.Tax_Rate,5,2) + '%'
    @ ROW,70 SAY InvHdr.Tax_Amt  PICTURE '9999999.99'
    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF  
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    @ ROW,12 SAY 'TOTAL - F R E I G H T'
    @ ROW,70 SAY WKAMT            PICTURE '9999999.99'
    ROW=ROW+1
  ENDIF

  *** Print the Tax rate and Tax amount 
  IF llTax AND lcTaxMeth = 'A'
    @ ROW,12 SAY lcTaxDesc
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY STR(InvHdr.Tax_Rate,5,2) + '%'
    @ ROW,70 SAY InvHdr.Tax_Amt  PICTURE '9999999.99'
    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF  
  ENDIF  
  @ 54,01 SAY lcPhone PICTURE lcPhonPict SIZE 1,16
  @ 54,18 SAY InvHdr.NOTE1 
  @ 55,18 SAY InvHdr.NOTE2
  IF !EMPTY(lcRpMsg1 + lcRpMsg2)
    @ 56,10 SAY lcRpMsg1
    @ 57,10 SAY lcRpMsg2
  ENDIF

  IF InvHdr.APPROVAL<>' ' AND UPPER(APPROVAL)<>'DEC'
    @ 58,56 SAY 'APPROVAL: ' + InvHdr.APPROVAL
  ENDIF
  @ 59,01 SAY InvHdr.PikTkt
  @ 59,09 SAY ShipDate
  @ 59,17 SAY CARTONS               PICTURE '@Z 999'
  @ 59,25 SAY WEIGHT                PICTURE '@Z 99999'
  @ 59,70 SAY InvHdr.TOTALCHG       PICTURE '9999999.99'
  SELECT InvHdr 

  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
    NEWDOC = .T.
  ENDIF
   
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (Invoice) VALUES (InvHdr.Invoice)
  ENDIF  
ENDSCAN
SET DEVICE TO SCREEN
RETURN
*-- End of Invoice form

*!**************************************************************************
*! Name      : lpGetScale
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/15/2002
*! Purpose   : Build a string holding 1st uniqe 5 scales from invoice lines.
*!**************************************************************************
*! Example   : DO lpGetScale
*!**************************************************************************
*
PROCEDURE lpGetScale
PARAMETERS lcInvNo

PRIVATE lnRecNo, lcAlias, lnCount

*-- Save the current alias.
lcAlias = ALIAS()

SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)
lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo AND lnCount <= 5
  IF !(SUBSTR(Scale,1,1) $ lcScalStr)
    lcScalStr = lcScalStr + SUBSTR(Style.Scale,1,1)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp,"InvLine")

*-- Select the old alias.
SELECT (lcAlias)
*-- End of lpGetScale

*!**************************************************************************
*! Name      : lpContPage
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/15/2002
*! Purpose   : Print the footer of page.
*!**************************************************************************
*! Example   : DO lpContPage
*!**************************************************************************
*
PROCEDURE lpContPage
PRIVATE lcAlias
lcAlias = ALIAS()
@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT InvHdr
@ 54,01 SAY lcPhone  PICTURE lcPhonPict SIZE 1,16
@ 54,18 SAY InvHdr.NOTE1 
@ 55,18 SAY InvHdr.NOTE2
IF !EMPTY(lcRpMsg1 + lcRpMsg2)
  @ 56,10 SAY lcRpMsg1
  @ 57,10 SAY lcRpMsg2
ENDIF
@ 59,01 SAY InvHdr.PikTkt
@ 59,09 SAY ShipDate
@ 59,19 SAY '***'
@ 59,25 SAY '*****'
@ 59,71 SAY '******.**'

SELECT (lcAlias)
*-- End of lpContPage

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/15/2002
*! Purpose   : Print the page header.
*!**************************************************************************
*! Example   : DO lpPrintHdr
*!**************************************************************************
*
PROCEDURE lpPrintHdr
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT InvHdr
@ 6,72 SAY xInvoice    
@ 09,16 SAY XBTNAME         && Line 9
IF CURLOOP = '1'
  @ 09,55 SAY XSTNAME
ENDIF

@ 10,16 SAY XBTADDR1        && Line 10
IF CURLOOP = '1'
  @ 10,55 SAY XSTADDR1
ENDIF

@ 11,16 SAY XBTADDR2        && Line 11
IF CURLOOP = '1'
  @ 11,55 SAY XSTADDR2
ENDIF

@ 12,16 SAY XBTADDR3
IF CURLOOP = '1'            && Line 12
  @ 12,55 SAY XSTADDR3
ENDIF
@ 16,01 SAY lcAccount
@ 16,10 SAY lcCusPo
@ 16,21 SAY IIF(CURLOOP='1',lcStore,'')
@ 16,29 SAY lcDept
@ 16,35 SAY lcOrder
@ 16,43 SAY SUBSTR(PTerms,1,12)
@ 16,56 SAY lcRep1
@ 16,60 SAY lcRep2
@ 16,63 SAY SUBSTR(lcShipDesc,1,12)
@ 16,76 SAY SUBSTR(Customer.Note,1,6)
*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)

DO lpGetScale WITH XInvoice
SELECT Scale
Row = 18
FOR lnI = 1 TO LEN(lcScalStr)
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  @ Row,23 SAY lcSizeScal
  @ Row,25 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ1),1,3),3,' ')
  @ Row,29 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ2),1,3),3,' ')
  @ Row,33 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ3),1,3),3,' ')
  @ Row,37 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ4),1,3),3,' ')
  @ Row,41 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ5),1,3),3,' ')
  @ Row,45 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ6),1,3),3,' ')
  @ Row,49 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ7),1,3),3,' ')
  @ Row,53 SAY PADL(SUBSTR(ALLTRIM(Scale.SZ8),1,3),3,' ')
  Row = Row + 1
ENDFOR

SELECT InvLine
XStore = Store
ROW = 23

SELECT (lcAlias)
*-- End of lpPrintHdr.