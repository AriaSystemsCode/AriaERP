*:***************************************************************************
*: Program file  : ARPINVIW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR INDUSTRIE WEAR, INC.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100789)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 8 SIZE SCALES
*: Date          : 02/11/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102198
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVIW
*:***************************************************************************
*B804130,1 [Start] Change Dz to UNITS This modification was done in NY Office and sent to Cairo
*C102454,1 SSE 11/25/2001 Custom adding allowance types to invoice for WalMart.
*B804562,1 SSE 12/26/2001 Fix bug of removing the discount line.
*B804558,1 SSE 12/26/2001 Fix bug of printing 12 char of custpo instead of 10 char.
*B605315,1 SSE 01/02/2001 Fix bug of printing an extra page in invoices.
*C127962,1 MHM 04/08/2005 AS Per customer req I chenge codes of allowance
*C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
*:***************************************************************************
           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
NEWDOC = .T.
MAXROW = 44
*--THE COLOR LENGTH
STORE "" TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnCd  = LEN(laItemSeg[lnCount,3])
    lnClrPosCd = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

               *--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
                  *--End Section to get the FromZone --*

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
  XINVOICE = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0 TO XPIECES, XSUBTOTAL,lnCartons,lnTotCtn,lnTWeight,Row
    STORE '' TO lcCustNote,lcSkuNo
    NEWDOC   = .F.
    XORDER   = ORDER
    XPHONE   = INVHDR.PHONE
    XNOTE1   = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2   = IIF(NOTE2<>'*', NOTE2, '')
    XACCOUNT = ACCOUNT
    XSTORE   = STORE

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       XBTADDR2 = laSoldTo[3]
       XBTADDR3 = ''
    ENDIF
    lcCustNote = CUSTOMER.Note

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
    lcPterms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcPshipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')

    DO CASE
      CASE 'G' $ lcUpsType
        XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        XZN = IIF(!EMPTY(XZN),'('+ XZN +')' ,XZN)
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE

    lcPshipVia = IIF(TRIM(lcPshipVia) = "N/A" , "" ,TRIM(lcPshipVia)) +XZN
    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    XFNAME  = lcFacName           && Factor company name.
    XFADDR1 = laFactor[1]         && Factor address#1.
    XFADDR2 = laFactor[2]
    XFADDR3 = laFactor[3]
    IF LEN(TRIM(laFactor[2])) =0  && If address#2 is empty fill it with address#3.
      XFADDR2 = laFactor[3]
      XFADDR3 = ''
    ENDIF
    CURLOOP = '1'                 && Variable to check if it is the first time for the new document.
  ENDIF
  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  XSTORE = STORE
  ROW    = 24
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
     SELECT INVLINE
     IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
       IF ROW >= MAXROW
         IF INVOICE <> XINVOICE
           EXIT
         ENDIF
         =lfGetcont()
         =lfGetInHdr()
         LOOP
       ENDIF
       EXIT
     ENDIF
     IF TOTQTY = 0
        SKIP
        LOOP
     ENDIF

    SELECT InvLine
    @ Row,01 SAY IIF(EMPTY(ALTSTYLE),LEFT(STYLE,lnLenth),LEFT(ALTSTYLE,lnLenth))
    @ Row,14 SAY IIF(EMPTY(ALTSTYLE),SUBSTR(STYLE,lnClrPosCd,lnClrLnCd),SUBSTR(ALTSTYLE,lnClrPosCd,lnClrLnCd))
    @ Row,23 SAY LEFT(STYLE.SCALE,1)
    @ Row,25 SAY STYLE.DESC
    @ Row,52 SAY TotQty PICTURE '99999'

    * SUM INVOICE TOTALS
    xLineTot   = Price * TotQty
    xPieces    = xPieces + TotQty
    xSubTotal  = xSubTotal+xLineTot

    @ Row,59 SAY PRICE PICTURE '9999.99'
    @ Row,70  SAY xLineTot  PICTURE '999999.99'

    IF !EMPTY(ALLTRIM(MLINE(Note_Mem,1)))
      Row = Row + 1
      @ Row,01 SAY MLINE(Note_Mem,1)
    ENDIF
    Row=Row+1

    IF Style.Qty_Ctn <> 0
      lnCartons = CEILING(TotQty/Style.Qty_Ctn)
    ELSE
      lnCartons = 0
    ENDIF
    lnTotCtn    =lnTotCtn  + lnCartons
    lnTWeight   =lnTWeight + (Style.nStyWeight * TotQty)
    = lfPrtSku()
    *B804130,1 [Start] This modification was done in NY Office and set to cairo
    * @ ROW,14 SAY ALLTRIM(STR(lnCartons)) + ' CTN  X  ' + ALLTRIM(STR(Style.Qty_Ctn)) + ' DZ.    ' + lcSkuNo
    @ ROW,14 SAY ALLTRIM(STR(lnCartons)) + ' CTN  X  ' + ALLTRIM(STR(Style.Qty_Ctn)) + ' UNITS  ' + lcSkuNo
    *B804130,1 [End] 
    IF Row >= MaxRow
      =lfGetcont()
      =lfGetInHdr()
    ELSE
      Row = Row + 1
    ENDIF

    * GET NEXT LINE ITEM
    SELECT InvLine
    SKIP
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(InvLine.Invoice = xInvoice ,'1','0')
  IF ENDPAGE = '1' .AND. !xNote_Loop
    =lfGetcont()
    LOOP
  ENDIF

  *C102454,1 Shift the printing of Notepad to be after Total Merchandise. [Begin]
  *** Print invoice Notepad.
  *IF llRpInvNot
  *  SELECT NotePad
  *  lnOldMemW = SET("MEMOWIDTH")
  *  SET MEMOWIDTH TO 75

  *  IF TYPE + KEY <> 'C' + xInvoice
  *    SEEK 'C' + xInvoice
  *  ENDIF
  *  lnMemLins = MEMLINES(NotePad.Mnotes)

  *  IF TYPE + KEY = 'C' + xInvoice
  *    @ Row,02 SAY '* -- N O T E S -- *'
  *    Row = Row + 1
  *    DO WHILE lnNotLine <= lnMemLins
  *      IF Row >= MaxRow
  *        xNote_Loop = .T.
  *        lnNotLine = lnNotLine - 1
  *      ELSE
  *        xNote_Loop = .F.
  *        @ Row,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
  *        Row = Row + 1
  *      ENDIF
  *      IF ROW >= MAXROW
  *        =lfGetcont()
  *        =lfGetInHdr()
  *      ENDIF
  *      lnNotLine = lnNotLine + 1
  *    ENDDO
  *    IF !xNote_Loop
  *      @ Row,02 SAY '* -- END OF NOTES -- *'
  *      lnNotLine = 1
  *      Row = Row + 1
  *    ELSE
  *      =lfGetcont()
  *      LOOP
  *    ENDIF
  *  ENDIF
  *  SET MEMOWIDTH TO lnOldMemW
  *ENDIF
  *C102454,1 Shift the printing of Notepad to be after Total Merchandise. [End]
  
  FOR lnCounter = 1 TO 3
    IF Row >= MaxRow
      =lfGetcont()
      =lfGetInHdr()
    ELSE
      Row = Row + 1
    ENDIF
    DO CASE
      CASE lnCounter = 1
        @ Row,12 SAY 'TOTAL WEIGHT       : '+PADL(STR(lnTWeight, 9, 2), 9) + ' LBS.'
      CASE lnCounter = 2
        @ Row,12 SAY 'TOTAL # OF CARTONS : '+PADL(lnTotCtn, 6)
      CASE lnCounter = 3
        @ Row,12 SAY lcCustNote
    ENDCASE
  ENDFOR
  Row = Row + 2
  IF Row >= MaxRow
    =lfGetcont()
    =lfGetInHdr()
  ENDIF

  SELECT InvHdr
  @ Row,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ Row,51 SAY xPieces PICTURE '999999'
  @ Row,69 SAY xSubTotal PICTURE '9999999.99'
  Row=Row+1
  
  *B804562,1 Commented out , customer needs to remove the discount. [Begin]
  *IF DISCOUNT<>0
  *  @ Row,12 SAY 'TOTAL - D I S C O U N T'
  *  @ Row,68 SAY Discount PICTURE '99999999.99'
  *  Row=Row+1
  *B804562,1 Commented out , customer needs to remove the discount. [End]

  *** Print the tax rate and tax amount
  IF xTax .AND. xTax_METH = 'M'
    XSTRING_RATE = STR(InvHdr.TAX_RATE,5,2)
    @ Row,12 SAY xTax_DESC
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF
    @ Row,60 SAY XSTRING_RATE + '%'
    @ Row,69 SAY InvHdr.TAX_AMT  PICTURE '9999999.99'
    Row = Row + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,60 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,69 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF
  ENDIF

  WKAMT = Freight + Insur + Cod
  IF WKAMT <> 0
    @ Row,12 SAY 'TOTAL - F R E I G H T'
    @ Row,69 SAY WKAMT PICTURE '9999999.99'
    Row=Row+1
  ENDIF

  *** Print the tax rate and tax amount
  IF xTax .AND. xTax_METH = 'A'
    xString_Rate = STR(InvHdr.Tax_Rate,5,2)
    @ Row,12 SAY xTax_DESC
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF
    @ Row,60 SAY xString_Rate + '%'
    @ Row,69 SAY InvHdr.Tax_Amt  PICTURE '9999999.99'
    Row = Row + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,60 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,69 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF
  ENDIF

  *C102454,1 Add the printing of Notepad to be after Total Merchandise. [Begin]

  *IF ROW >= MAXROW
  *  =lfGetcont()
  *  =lfGetInHdr()
  *ENDIF
  *Row = Row + 1

  *** Print invoice Notepad.
  IF llRpInvNot
    
    SELECT NotePad
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + xInvoice
      SEEK 'C' + xInvoice
    ENDIF
    lnMemLins = MEMLINES(NotePad.Mnotes)

    IF TYPE + KEY = 'C' + xInvoice
      
      *B605315,1 Check to print an extra page if we found notepad. [Begin]
      IF ROW >= MAXROW
        =lfGetcont()
        =lfGetInHdr()
      ENDIF
      Row = Row + 1
      *B605315,1 Check to print an extra page if we found notepad. [End]
    
      @ Row,02 SAY '* -- N O T E S -- *'
      Row = Row + 1
      DO WHILE lnNotLine <= lnMemLins
        IF Row >= MaxRow
          xNote_Loop = .T.
          lnNotLine = lnNotLine - 1
        ELSE
          xNote_Loop = .F.
          @ Row,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          Row = Row + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !xNote_Loop
        @ Row,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        Row = Row + 1
      ELSE
        =lfGetcont()
        LOOP
      ENDIF      
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  *B605315,1 SSE Commented out [Begin]
  *IF ROW >= MAXROW
  *  =lfGetcont()
  *  =lfGetInHdr()
  *ENDIF
  *Row = Row + 1
  *B605315,1 SSE Commented out [End]
  *MMR
  *C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
  *IF (XAccount = "W1000" OR XAccount = "W1100") AND ABS(ROUND(InvHdr.Discount/xSubTotal,3)) = 0.025   
 IF (XAccount = "W1000" OR XAccount = "W1100")  AND !EMPTY(laRpTarget)   
  *C127962,2 MMR.[End]
    IF ROW > 37
      =lfGetcont()
      =lfGetInHdr()
    ENDIF
    *C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
    *FOR lnCounter = 1 TO 7
    *  DO CASE 
    *    CASE lnCounter = 1
    *      @ Row,12 SAY 'Code'
    *      @ Row,20 SAY 'Allowances'
    *      @ Row,58 SAY 'Percent'
    *      Row = Row + 1                      
    *    CASE lnCounter = 2
    *      @ Row,12 SAY '----'
    *      @ Row,20 SAY '----------'
    *      @ Row,58 SAY '-------'
    *      Row = Row + 1   
    *    CASE lnCounter = 3
    *      *ash1 05/20/2005
    *      *IF order <> '102211'
    *        *--mhm2005*C127962,1 MHM 
    *        *@ Row,12 SAY '054'
    *        @ Row,12 SAY 'A E750'
    *        *--mhm2005*C127962,1 MHM 
    *        @ Row,20 SAY 'Warehouse Allowance'
    *        @ Row,62 SAY '1.0' 
    *        @ Row,68 SAY ROUND(xSubTotal * 0.01,2) PICTURE '99999999.99'
    *        Row = Row + 1                      
    *      *endif
    *    CASE lnCounter = 4
    *      *--mhm2005*C127962,1 MHM 
    *      *@ Row,12 SAY '055'
    *      @ Row,12 SAY 'A I570'
    *      *--mhm2005*C127962,1 MHM 
    *      @ Row,20 SAY 'New Location Allowance'
    *      @ Row,62 SAY '1.0'
    *      @ Row,68 SAY ROUND(xSubTotal * 0.01,2) PICTURE '99999999.99'
    *      Row = Row + 1                      
    *    CASE lnCounter = 5
    *      *--mhm2005*C127962,1 MHM 
    *      *@ Row,12 SAY '150'
    *      @ Row,12 SAY 'A C000'
    *      *--mhm2005*C127962,1 MHM 
    *      @ Row,20 SAY 'Softgoods Defective Allowance'
    *      @ Row,62 SAY '0.5'
    *      @ Row,68 SAY ABS(InvHdr.Discount) - (ROUND(xSubTotal * 0.01,2) * 2) PICTURE '99999999.99'
    *      Row = Row + 1                      
    *    CASE lnCounter = 6
    *      Row = Row + 1                      
    *    CASE lnCounter = 7
    *      @ Row,12 SAY 'Total Allowance'
    *      @ Row,68 SAY ABS(InvHdr.Discount) PICTURE '99999999.99'
    *      Row = Row + 1                      
    *  ENDCASE          
    *ENDFOR
    DIMENSION laCopy[1,1]
    STORE 0 TO lnPer,lnTotPer
    laCopy=''
    @ Row,12 SAY 'Code'
    @ Row,20 SAY 'Allowances'
    @ Row,58 SAY 'Percent'
    Row = Row + 1                      
    @ Row,12 SAY '----'
    @ Row,20 SAY '----------'
    @ Row,58 SAY '-------'
    Row = Row + 1   
    =lfGetCodes() 
    FOR lnCounter = 1 TO ALEN(laCopy,1)
        lnPer    = VAL(laCopy(lnCounter,3))/100
        lnTotPer = lnTotPer+ABS(ROUND(xSubTotal*lnPer,2))
        @ Row,12 SAY laCopy(lnCounter,1)
        @ Row,20 SAY laCopy(lnCounter,2)
        @ Row,62 SAY ALLTRIM(STR(ROUND(VAL(laCopy(lnCounter,3)),1),LEN(laCopy(lnCounter,3)),1))
        @ Row,68 SAY ROUND(xSubTotal*lnPer,2) PICTURE '99999999.99'
        Row = Row + 1   
    ENDFOR         
    @ Row,12 SAY 'Total Allowance'
    @ Row,68 SAY lnTotPer PICTURE '99999999.99'
   Row = Row + 1                      
 ENDIF
 *C127962,2 MMR.[End]
  *C102454,1 Add the printing of Notepad to be after Total Merchandise. [End]

  @ 51,01 SAY XPHONE SIZE 1,16
  @ 51,18 SAY xNote1 + ' ' + xNote2

  IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
     @ 52,10 SAY lcRpMsg1
     @ 53,10 SAY lcRpMsg2
  ENDIF

  *C102454,1 Add invoice header file to Approval field. [Begin]
  *IF InvHdr.Approval<>' ' .AND. UPPER(Approval)<>'DEC'
  IF InvHdr.Approval<>' ' .AND. UPPER(InvHdr.Approval)<>'DEC'
  *C102454,1 Add invoice header file to Approval field. [End]

     @ 54,56 SAY 'Approval: ' + InvHdr.Approval
  ENDIF

  @ 57,02 SAY DATE()
  @ 57,35 SAY InvHdr.Invoice
  *C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
  *@ 57,68 SAY InvHdr.TotalChg PICTURE '9999999.99'
  IF (XAccount = "W1000" OR XAccount = "W1100")  AND !EMPTY(laRpTarget)
    @ 57,68 SAY (xSubTotal-lnTotPer)+(InvHdr.Tax_Amt+InvHdr.nPSTAmt+ WKAMT) PICTURE '9999999.99'
  ELSE
    @ 57,68 SAY InvHdr.TotalChg PICTURE '9999999.99'
  ENDIF   
  *C127962,2 MMR.[End]      
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
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/11/2001
*! Purpose   : Build a string holding the first uniqe 5 scales
*!           : from the invoice line scal.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpGtScl()
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo

PRIVATE lnRecNo, lcAlias, lnCount

*-- Save the current alias.
lcAlias = ALIAS()
SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)
lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( LEFT(Scale,1) $ lcScalStr )
    lcScalStr = lcScalStr + LEFT(Scale,1)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp,"InvLine")

SELECT (lcAlias)

*-- End of lpGtScl.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 02/11/2001
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVIW.PRG
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)

*-- End of gfGetZone.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : Bassem Rafaat (BWA)
*! Date      : 02/11/2001
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
@ 02,40 SAY InvDate
@ 02,50 SAY 'INVOICE: '+xInvoice

*B804558,1 Print 12 char instead of 10 char. [Begin]
*@ 02,68 SAY 'PO: '+LEFT(INVHDR.CUSTPO,10)
@ 02,68 SAY 'PO: '+LEFT(INVHDR.CUSTPO,12)
*B804558,1 Print 12 char instead of 10 char. [End]

IF !EMPTY(lcDunsNo)
  @ 03,50 SAY 'DUNS: '+ ALLTRIM(lcDunsNo)
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 05,50 SAY XFNAME
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 06,50 SAY XFADDR1
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 07,50 SAY XFADDR2
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 08,50 SAY XFADDR3
ENDIF

@ 10,10 SAY XBTNAME
IF CURLOOP = '1'
  @ 10,47 SAY XSTNAME
ENDIF

@ 11,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 11,47 SAY XSTADDR1
ENDIF

@ 12,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 12,47 SAY XSTADDR2
ENDIF

@ 13,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ 13,47 SAY XSTADDR3
ENDIF

@ 17,01 SAY ACCOUNT

*B804558,1 Print 12 char instead of 10 char. [Begin]
*@ 17,09 SAY LEFT(CUSTPO,10)
@ 17,08 SAY LEFT(CUSTPO,12)
*B804558,1 Print 12 char instead of 10 char. [Begin]

@ 17,21 SAY IIF(CURLOOP='1',xStore,'')
@ 17,29 SAY DEPT
@ 17,34 SAY ORDER
@ 17,42 SAY LEFT(lcPterms,14)
@ 17,57 SAY Rep1
@ 17,65 SAY LEFT(lcPshipVia,15)
ROW = 24

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/11/2001
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

@ Row+1,12 SAY 'C O N T I N U E D ...'
@ 51,01 SAY XPHONE SIZE 1,16
@ 51,18 SAY InvHdr.Note1 + ' ' + InvHdr.Note2

IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 52,10 SAY lcRpMsg1
  @ 53,10 SAY lcRpMsg2
ENDIF
@ 57,01 SAY DATE()
@ 57,35 SAY xInvoice
@ 57,69 SAY '******.**'

*--End of lfGetcont.
*!*************************************************************
*! Name      : lfPrtSku
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/11/2001
*! Purpose   : To get the sku # of the style/color if exist.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtSku()
*!*************************************************************
FUNCTION lfPrtSku

lnAlias=SELECT()
lcSkuNo = ''
IF SEEK('S'+InvLine.Account+Invline.Style,'Spck_Lin')
  =SEEK('M'+InvLine.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  ELSE
    lnDime1 = 8
  ENDIF
  lcSkuNo = 'SKU#: ' + SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)
ENDIF
SELECT (lnAlias)

*--End of lfPrtSku.

*!*************************************************************
*! Name      : lfFillAll
*! Developer : Mostafa Rawash (MMR)
*! Date      : 08/21/2005
*! Purpose   : Function to fill Allowance
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll
PARAMETER lcParam
*C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
DIMENSION laRpSource[1,1],laRpTarget[1,1]

STORE '' TO laRpSource,laRpTarget

*--The ALLOWANCE.
SELECT CODES
SELECT DISTINCT cCode_NO FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CALLOWANCE ' AND crltField = 'Y';
                                        INTO ARRAY laRpSource
*-- End of lfFillAll.
*!*************************************************************
*! Name      : lfCrtMover
*! Developer : Mostafa Rawash (MMR)
*! Date      : 08/21/2005
*! Purpose   : Function to create mover for allowance ,
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfCrtMover()
*!*************************************************************
FUNCTION lfCrtMover
PARAMETER lcParam
*C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
= gfMover(@laRpSource,@laRpTarget,'Allowance ',.T.,'')
lnCount=0
FOR lnInd = 1 TO ALEN(laRpTarget)
    lnCount=lnCount+1
    IF lnCount>3
      =gfModalGen('INM00000B00000','','','','You have selected more than 3 Allowance Codes,your selections will be removed.')    
      DIMENSION laRpSource[1,1],laRpTarget[1,1]
      STORE '' TO laRpSource,laRpTarget
      SELECT CODES
      SELECT DISTINCT cCode_NO FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CALLOWANCE ' AND crltField = 'Y';
                                               INTO ARRAY laRpSource
      EXIT
    ENDIF 
ENDFOR
CLEAR READ
lcParam = .T.
RETURN lcParam

*-- End of lfCrtMover.
*!*************************************************************
*! Name      : lfGetCodes
*! Developer : Mostafa Rawash (MMR)
*! Date      : 08/21/2005
*! Purpose   : Function to Get Codes Desc and percenatge
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
*C127962,2 MMR 08/21/2005 Add ALLOWANCE in the codes and add mover with them in the invoice OG.
PRIVATE lcAlias,lcOrder
STORE 0 TO lnDim,lnCount
lnDim=ALEN(laRpTarget,1)
DIMENSION laCopy[lnDim,3]
lcAlias=ALIAS()
SELECT CODES
lcOrder=ORDER()
SET ORDER TO CODES
FOR lnCount=1 TO ALEN(laRpTarget,1)
  SEEK 'N'+laRpTarget(1,lnCount)
   laCopy(lnCount,1)=CODES.ccode_no
   laCopy(lnCount,2)=CODES.cdiscrep 
   SKIP
   laCopy(lnCount,3)=CODES.crltd_vlu
ENDFOR
SET ORDER TO (lcOrder)
SELECT (lcAlias)
*-- End of lfGetCodes.

