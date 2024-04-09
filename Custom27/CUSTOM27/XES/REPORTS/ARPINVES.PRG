*:***************************************************************************
*: Program file  : ARPINVES.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR X.E.S.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100716)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*: Date          : 12/10/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102068
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVES
*:***************************************************************************

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

NEWDOC = .T.
MAXROW = 47

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF

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
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = INVHDR.PHONE
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)

    =lfSolSpAdr()
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       XBTADDR2 = laSoldTo[3]
       XBTADDR3 = ''
    ENDIF
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
    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
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
    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN
    CURLOOP = '1'
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
   XSCALE =  ' '
   ROW    = 21
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
     SELECT INVLINE
     @ ROW,00 SAY SUBSTR(IIF(!EMPTY(ALTSTYLE),ALTSTYLE,STYLE),1,7)
     @ ROW,08 SAY SUBSTR(IIF(!EMPTY(ALTCOLOR),ALTCOLOR,STYLE),14,6)
     @ ROW,15 SAY SUBSTR(STYLE.DESC,1,17)
     @ ROW,33 SAY LEFT(STYLE.SCALE,1)
     @ ROW,34 SAY QTY1   PICTURE '@Z 9999'
     @ ROW,38 SAY QTY2   PICTURE '@Z 9999'
     @ ROW,42 SAY QTY3   PICTURE '@Z 9999'
     @ ROW,46 SAY QTY4   PICTURE '@Z 9999'
     @ ROW,50 SAY QTY5   PICTURE '@Z 9999'
     @ ROW,54 SAY QTY6   PICTURE '@Z 9999'
     @ ROW,58 SAY TOTQTY PICTURE '99999'

     * CUM INVOICE TOTALS
     XLINETOT   = PRICE * TOTQTY
     XPIECES    = XPIECES + TOTQTY
     XSUBTOTAL  = XSUBTOTAL+XLINETOT
     @ ROW,64  SAY PRICE     PICTURE '9999.99'
     @ ROW,73  SAY XLINETOT  PICTURE '999999.99'
     DO lpPrnLbl
     ROW = ROW + 1
     
     * GET NEXT LINE ITEM
     SELECT INVLINE
     SKIP
   ENDDO
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
     =lfGetcont()
     LOOP
   ENDIF

  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + XINVOICE
      @ ROW,02 SAY '* -- N O T E S -- *' 
      DO lpPrnLbl
      ROW = ROW + 1
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          DO lpPrnLbl
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

  SELECT INVHDR
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,59 SAY XPIECES              PICTURE '999999'
  @ ROW,72 SAY XSUBTOTAL            PICTURE '9999999.99'
  DO lpPrnLbl
  ROW=ROW+1

  IF DISCOUNT<>0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,71 SAY DISCOUNT         PICTURE '99999999.99'
    DO lpPrnLbl
    ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF XTAX .AND. XTAX_METH = 'M' 
    XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
    @ ROW,12 SAY XTAX_DESC
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY XSTRING_RATE + '%'
    @ ROW,72 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    DO lpPrnLbl
    ROW = ROW + 1     
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      DO lpPrnLbl
      Row = Row + 1       
    ENDIF  
  ENDIF  
  
  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
     @ ROW,72 SAY WKAMT PICTURE '9999999.99'
     DO lpPrnLbl
     ROW=ROW+1      
  ENDIF

  *** Print the tax rate and tax amount 
  IF XTAX .AND. XTAX_METH = 'A'
    XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
    @ ROW,12 SAY XTAX_DESC
    IF !EMPTY(lcTaxRefr)
      @ Row,33 SAY lcTaxRefr
    ENDIF  
    @ ROW,64 SAY XSTRING_RATE + '%'
    @ ROW,72 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    DO lpPrnLbl
    ROW = ROW + 1     
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      DO lpPrnLbl
      Row = Row + 1       
    ENDIF  
  ENDIF  
  IF ROW < 51
    lcRow1=ROW
    FOR I = lcRow1 TO 50
      ROW = ROW + 1
      DO lpPrnLbl
    ENDFOR
  ENDIF
  @ 51,01 SAY XPHONE SIZE 1,16
  @ 51,18 SAY XNOTE1 + ' ' + XNOTE2
  IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
    @ 53,10 SAY lcRpMsg1
    @ 54,10 SAY lcRpMsg2
  ENDIF
  DO lpSidLbl  WITH 54
  DO lpSidLbl1 WITH 55
      
  IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
    @ 56,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
  ENDIF
  DO lpSidLbl2 WITH 56

  @ 57,02 SAY INVHDR.PIKTKT
  @ 57,09 SAY ShipDate
  @ 57,19 SAY CARTONS PICTURE '@Z 999'
  @ 57,25 SAY WEIGHT PICTURE '@Z 99999'
  @ 57,35 SAY INVHDR.INVOICE
  @ 57,72 SAY INVHDR.TOTALCHG PICTURE '9999999.99'
  DO lpSidLbl3 WITH 57
  DO lpSidLbl4 WITH 61

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
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT 
*! Date      : 12/10/2000
*! Purpose   : Print the style scales.
*!*************************************************************
*! Called from : ARPINVES.PRG
*!*************************************************************
*! Calls     : ....
*!*************************************************************
*! Passed Parameters : lcInvNo
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpGtScl
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo
PRIVATE lnRecNo, lcAlias, lnCount

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
*! Name      : lpPrnLbl
*! Developer : BASSEM RAFAAT 
*! Date      : 12/10/2000
*! Purpose   : Print the side labels.
*!*************************************************************
*! Called from : ARPINVES.PRG
*!*************************************************************
*! Calls       : lpSidLbl , lpSidLbl1 , lpSidLbl2 , lpSidLbl3
*!             : lpSidLbl4
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpPrnLbl
*!*************************************************************
PROCEDURE lpPrnLbl

IF ROW=21 OR ROW=38
  DO lpSidLbl WITH ROW
ENDIF

IF ROW=22 OR ROW=39
  DO lpSidLbl1 WITH ROW
ENDIF

IF ROW=23 OR ROW=40
  DO lpSidLbl2 WITH ROW
ENDIF

IF ROW=24 OR ROW=41
  DO lpSidLbl3 WITH ROW
ENDIF

IF ROW=28 OR ROW=45
  DO lpSidLbl4 WITH ROW
ENDIF

*-- End of lpPrnLbl.
*!*************************************************************
*! Name      : lpSidLbl1,2,3,4
*! Developer : BASSEM RAFAAT 
*! Date      : 12/10/2000
*! Purpose   : Print the side labels.
*!*************************************************************
*! Called from : ARPINVES.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : lcRow
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpSidLbl
*!*************************************************************
PROCEDURE lpSidLbl
PARAMETER lcRow

@lcRow,87  SAY SUBSTR(xSTName,1,22)
@lcRow,115 SAY SUBSTR(xSTName,1,20)

PROCEDURE lpSidLbl1
PARAMETER lcRow
@lcRow,87 SAY SUBSTR(xSTAddr1,1,22)
@lcRow,115 SAY SUBSTR(xSTAddr1,1,20)

PROCEDURE lpSidLbl2
PARAMETER lcRow
@lcRow,87  SAY SUBSTR(xSTAddr2,1,22)
@lcRow,115 SAY SUBSTR(xSTAddr2,1,20)

PROCEDURE lpSidLbl3
PARAMETER lcRow
@lcRow,87 SAY SUBSTR(xSTAddr3,1,22)
@lcRow,115 SAY SUBSTR(xSTAddr3,1,20)

PROCEDURE lpSidLbl4
PARAMETER lcRow
@lcRow,85  SAY LEFT(InvHdr.CUSTPO,10)
@lcRow,96  SAY InvHdr.DEPT
@lcRow,103 SAY InvHdr.INVOICE
@lcRow,111 SAY LEFT(InvHdr.CUSTPO,10)
@lcRow,122 SAY InvHdr.DEPT
@lcRow,129 SAY InvHdr.INVOICE

*-- End OF lpSidLbl1,2,3,4.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 12/10/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : ARPINVES.PRG
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
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+ LEFT(INVHDR.CUSTPO,10)

IF llPrntComp
  @ 03,05 SAY HLINE1
ENDIF

IF llPrntComp
  @ 04,05 SAY HLINE2
ENDIF

IF !EMPTY(CUSTOMER.DUNS)
  @ 04,37 SAY 'DUNS: '+ ALLTRIM(CUSTOMER.DUNS)
ENDIF

IF llPrntComp
  @ 05,05 SAY HLINE3
ENDIF
   
IF llPrntComp
  @ 06,05 SAY HLINE4
ENDIF

IF llPrntComp
  @ 07,05 SAY lcCompPhon SIZE 1,16
ENDIF

@ 08,10 SAY XBTNAME
IF CURLOOP = '1'
  @ 08,47 SAY XSTNAME
ENDIF
DO lpSidLbl WITH 8

@ 09,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 09,47 SAY XSTADDR1
ENDIF
DO lpSidLbl1 WITH 9

@ 10,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 10,47 SAY XSTADDR2
ENDIF
DO lpSidLbl2 WITH 10

@ 11,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ 11,47 SAY XSTADDR3
ENDIF
DO lpSidLbl3 WITH 11   

@ 14,01 SAY ACCOUNT
@ 14,09 SAY LEFT(CUSTPO,10)
@ 14,21 SAY IIF(CURLOOP='1',XSTORE,'')
@ 14,30 SAY DEPT
@ 14,36 SAY ORDER
@ 14,44 SAY LEFT(PTERMS,14)
@ 14,60 SAY REP1
@ 14,64 SAY REP2
@ 14,68 SAY PSHIPVIA

*------------------
* PRINT SCALES
*------------------

lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 16
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  @ Row,33 SAY lcSizeScal
  @ Row,35 SAY PADL(SCALE.SZ1,3,' ')
  @ Row,39 SAY PADL(SCALE.SZ2,3,' ')
  @ Row,43 SAY PADL(SCALE.SZ3,3,' ')
  @ Row,47 SAY PADL(SCALE.SZ4,3,' ')
  @ Row,51 SAY PADL(SCALE.SZ5,3,' ')
  @ Row,55 SAY PADL(SCALE.SZ6,3,' ')
  IF ROW = 16
    DO lpSidLbl4 WITH 16
  ENDIF
  Row = Row + 1
ENDFOR

ROW    = 21
*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 12/10/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : ARPINVES.PRG
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
SELECT INVHDR
@ 51,01 SAY XPHONE SIZE 1,16
@ 51,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 53,10 SAY lcRpMsg1
  @ 54,10 SAY lcRpMsg2
ENDIF
DO lpSidLbl  WITH 54
DO lpSidLbl1 WITH 55
DO lpSidLbl2 WITH 56
@ 57,02 SAY INVHDR.PIKTKT
@ 57,09 SAY ShipDate
@ 57,19 SAY '***'
@ 57,25 SAY '*****'
@ 57,35 SAY XINVOICE
@ 57,73 SAY '******.**'
DO lpSidLbl3 WITH 57
DO lpSidLbl4 WITH 61

*-- End OF lfGetcont.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 12/10/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVES.PRG
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
