*:***************************************************************************
*: Program file  : ARPINVUM.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR YOU & ME NATURALLY.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100595)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11"
*: Date          : 07/02/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102179
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVUM
*:***************************************************************************

           *--Section of Initializing the variables --*
llNote_Loop = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine   = 1        && For the notepad
llNoRec     = .F.      && Variable used to make the endreport function in the main program.

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
NEWDOC    = .T.
MAXROW    = 47
lnInvNo   = 22
STORE 0   TO lnLinRow , MaxRow , lnHdrRow , lnBotRow

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

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

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
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE

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
    lcTerms  = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcPshipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')

    DO CASE
      CASE 'G' $ lcUpsType
        XZN  = IIF(!EMPTY(INVHDR.UPSZONE),'('+INVHDR.UPSZONE+')','')
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

  DO WHILE CURLOOP = '1' .AND. !llNote_Loop
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. lnLinRow >= MAXROW
      IF lnLinRow >= MAXROW
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
    
    lcOldPack = ''
    SELECT INVLINE
    IF !EMPTY(INVLINE.Pack_Id) .AND. lcOldPack <> INVLINE.Pack_Id
      @ lnLinRow,05 SAY 'PACK_ID : ' + INVLINE.Pack_Id
      lcOldPack = INVLINE.Pack_Id
      lnLinRow  = lnLinRow + 1
    ENDIF

    @ lnLinRow,05 SAY LEFT(STYLE,7)
    @ lnLinRow,15 SAY SUBSTR(STYLE,lnClrPosCd,3)
    @ lnLinRow,24 SAY STYLE.DESC
    @ lnLinRow,49 SAY QTY1   PICTURE '@Z 9999'
    @ lnLinRow,54 SAY QTY2   PICTURE '@Z 9999'
    @ lnLinRow,59 SAY QTY3   PICTURE '@Z 9999'
    @ lnLinRow,64 SAY QTY4   PICTURE '@Z 9999'
    @ lnLinRow,69 SAY QTY5   PICTURE '@Z 9999'
    @ lnLinRow,74 SAY QTY6   PICTURE '@Z 9999'
    @ lnLinRow,79 SAY QTY7   PICTURE '@Z 9999'
    @ lnLinRow,84 SAY TOTQTY PICTURE '99999'

    * SUM INVOICE TOTALS
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    @ lnLinRow,090 SAY PRICE     PICTURE '9999.99'
    @ lnLinRow,098 SAY XLINETOT  PICTURE '999999.99'

    lnLinRow = lnLinRow + 1

    IF SEEK('S'+INVLINE.Account+INVLINE.Style ,'Spck_Lin')
      @ lnLinRow,005 SAY "CUSTOMER SKU: " +;
                     LEFT(IIF(!EMPTY(Spck_Lin.Sku),Spck_Lin.Sku,Spck_Lin.Pack_Id),8)
      lnLinRow = lnLinRow + 1
    ENDIF

    *-- GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' .AND. !llNote_Loop
    =lfGetcont()
    LOOP
  ENDIF
  lnLinRow = lnLinRow + 1

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
      @ lnLinRow,05 SAY '* -- I N V O I C E  N O T E S -- *'
      lnLinRow = lnLinRow + 1
      DO WHILE lnNotLine <= lnMemLins
      IF lnLinRow >= MaxRow
          llNote_Loop = .T.
        ELSE
          llNote_Loop = .F.
          @ lnLinRow,05 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          lnLinRow = lnLinRow + 1
        ENDIF
        IF lnLinRow >= MaxRow
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !llNote_Loop
        @ lnLinRow,05 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        lnLinRow = lnLinRow + 1
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT INVHDR
  IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
    @ lnBotRow,014 SAY 'FACTOR AUTHORIZATION NUMBER : ' + InvHdr.Approval
  ELSE
    @ lnBotRow,014 SAY 'PAYABLE TO YOU AND ME NATURALLY INC.'
  ENDIF

  IF DISCOUNT<>0
    @ lnBotRow,084 SAY 'DISCOUNT'
    @ lnBotRow,098 SAY DISCOUNT  PICTURE '999999.99'
  ENDIF
  lnBotRow = lnBotRow + 1

  IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
    @ lnBotRow,014 SAY XFNAME
  ELSE
    @ lnBotRow,014 SAY LEFT(HLINE1,20) +;
                       LEFT(HLINE2,20) +;
                       LEFT(HLINE3,20)
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    @ lnBotRow,084 SAY 'FREIGHT'
    @ lnBotRow,096 SAY WKAMT  PICTURE '99999999.99'
  ENDIF
  lnBotRow = lnBotRow + 1

  IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
    @ lnBotRow,014 SAY LEFT(ALLTRIM(XFADDR1),20) + ' '+;
                       LEFT(ALLTRIM(XFADDR2),20) + ' '+;
                       LEFT(ALLTRIM(XFADDR3),20)
  ELSE
    @ lnBotRow,014 SAY 'THANK YOU FOR YOUR ORDERS!'
  ENDIF
  @ lnBotRow,095 SAY INVHDR.TOTALCHG  PICTURE '999999999.99'

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
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2001
*! Purpose   : Build a string holding the first uniqe 2 scales
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
PARAMETERS lcInvNo, lnHowMany

PRIVATE lnRecNo, lnAlias, lnCount

lnAlias = ALIAS()
SELECT INVLINE

lnRecNo = RECNO()
lnCount = 1

SEEK lcInvNo
SCAN WHILE INVLINE.INVOICE = lcInvNo .AND. lnCount <= lnHowMany
  IF !( LEFT(Scale,1) $ lcScalStr )
    lcScalStr = lcScalStr + LEFT(Scale,1)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN

IF RECCOUNT() >= lnRecNo
  GOTO lnRecNo
ENDIF

SELECT (lnAlias)

*--End of lpGtScl.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2001
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
IF lnInvNo = 22
  lnHdrRow = 02                   && Start header row.
  lnLinRow = 21                   && Start lines printing row.
  lnBotRow = 46                   && Start footer row.
  MaxRow   = lnBotRow - 3         && Max row number for lines printing.
  lnInvNo  = 0
ELSE
  lnHdrRow = lnBotRow + 05        && Start header row.
  lnLinRow = lnHdrRow + 19        && Start lines printing row.
  lnBotRow = lnLinRow + 25        && Start footer row.
  MaxRow   = lnBotRow - 03        && Max row number for lines printing.
  lnInvNo  = lnInvNo  + 01
ENDIF

IF llPrntComp
  @ lnHdrRow,04 SAY HLINE1
ENDIF

lnHdrRow = lnHdrRow + 1
IF llPrntComp
  @ lnHdrRow,04 SAY HLINE2
ENDIF

lnHdrRow = lnHdrRow + 1
IF llPrntComp
  @ lnHdrRow,04 SAY HLINE3
ENDIF

lnHdrRow = lnHdrRow + 1
IF llPrntComp
  @ lnHdrRow,04 SAY HLINE4
ENDIF

lnHdrRow = lnHdrRow + 1
IF llPrntComp
  @ lnHdrRow,04 SAY TRANSFORM(lcCompPhon , lcPhonPict)
ENDIF

lnHdrRow = lnHdrRow + 3
@ lnHdrRow,08 SAY LEFT(XBTNAME,22) + ' ' + Account
IF CURLOOP = '1'
  @ lnHdrRow,43 SAY LEFT(XSTNAME,22) + ' ' + Account
ENDIF

@ lnHdrRow,83 SAY XINVOICE
@ lnHdrRow,95 SAY ShipDate

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY XBTADDR1
IF CURLOOP = '1'
  @ lnHdrRow,43 SAY XSTADDR1
ENDIF

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY XBTADDR2
IF CURLOOP = '1'
  @ lnHdrRow,43 SAY XSTADDR2
ENDIF

lnHdrRow = lnHdrRow + 1
@ lnHdrRow,08 SAY XBTADDR3
IF CURLOOP = '1'
  @ lnHdrRow,43 SAY XSTADDR3
ENDIF

@ lnHdrRow,83 SAY PIKTKT
@ lnHdrRow,95 SAY ORDER

lnHdrRow = lnHdrRow + 5
@ lnHdrRow,006 SAY Account
@ lnHdrRow,017 SAY LEFT(lcTerms,14)
@ lnHdrRow,036 SAY DueDate
@ lnHdrRow,046 SAY REP1
@ lnHdrRow,055 SAY LEFT(lcPshipVia,15)
@ lnHdrRow,074 SAY LEFT(CUSTPO,10)
@ lnHdrRow,086 SAY DEPT
@ lnHdrRow,095 SAY Cartons
@ lnHdrRow,102 SAY Weight

*-- [1] LINE ITEM PRINT LOOP
SELECT INVLINE
XSCALE   =  ' '

*-- PRINT SCALE
lnHdrRow = lnHdrRow + 2

lcScalStr = SPACE(0)
lcInvNo   = INVHDR.Invoice
DO lpGtScl WITH lcInvNo, 2
  FOR lnI = 1 TO LEN( lcScalStr )
    lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
    =SEEK('S'+lcSizeScal,'Scale')
    @ lnHdrRow,49 SAY LEFT(ALLTRIM(Scale.Sz1),3)
    @ lnHdrRow,55 SAY LEFT(ALLTRIM(Scale.Sz2),3)
    @ lnHdrRow,60 SAY LEFT(ALLTRIM(Scale.Sz3),3)
    @ lnHdrRow,65 SAY LEFT(ALLTRIM(Scale.Sz4),3)
    @ lnHdrRow,70 SAY LEFT(ALLTRIM(Scale.Sz5),3)
    @ lnHdrRow,75 SAY LEFT(ALLTRIM(Scale.Sz6),3)
    @ lnHdrRow,80 SAY LEFT(ALLTRIM(Scale.Sz7),3)
    lnHdrRow = lnHdrRow + 1
  ENDFOR

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2001
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

@ lnLinRow+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR

*--End of lfGetcont.