*:***************************************************************************
*: Program file  : ARPINVAL.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR ALLEGRO.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES 
*! Date          : 01/31/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102028
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVAL
*:***************************************************************************
*:Modifications:
*:B804057,1 BWA 18/04/2001 Remove all from the code.
*:***************************************************************************

llnOTELOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
NEWDOC = .T.
MAXROW = 47

*B804057,1 BWA 18/04/2001 Remove all from the code.[START]
IF !EMPTY(lcEach) AND !EMPTY(lcDozn)
  lcRpSpEh = lcEach
  lcRpSpDz = lcDozn
ENDIF

IF !EMPTY(lcRpSpEh) OR !EMPTY(lcRpSpDz)
  IF LEN(lcRpSpEh) = 1
    lcRpSpEh = SUBSTR(laRpSpMan[VAL(lcRpSpEh),1],32,6)
  ENDIF
  IF LEN(lcRpSpDz) = 1
    lcRpSpDz = SUBSTR(laRpSpMan[VAL(lcRpSpDz),1],32,6)
  ENDIF
ENDIF
*B804057,1 [END]

*--Saving the variables to the mem file.
IF !EMPTY(lcRpSpEh) OR !EMPTY(lcRpSpDz)
  SAVE TO gcDataDir+lcRpFile+'.MEM' ALL LIKE lcRpSp*
ENDIF

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
    STORE 0.00 TO lnTotal , lnTotQty , lnSubTotal
    NEWDOC    = .F.
    XPHONE    = INVHDR.PHONE
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE

    *-- GET THE BILL TO AND SHIP ADDRESS
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

    *-- GET THE DESCRIPTION ABOUT THE CODES.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')

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
  ROW    = 26
  DO WHILE CURLOOP = '1' .AND. !llnOTELOOP
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
    lcColoral =  gfCodDes(SUBSTR(INVLINE.STYLE,lnClrPosCd,lnClrLnCd),'COLOR')
    @ ROW,01 SAY SUBSTR(INVLINE.STYLE,1,7)
    @ ROW,08 SAY SUBSTR(STYLE.DESC,1,14)
    @ ROW,23 SAY QTY1   PICTURE '@Z 99999'
    @ ROW,29 SAY QTY2   PICTURE '@Z 99999'
    @ ROW,35 SAY QTY3   PICTURE '@Z 99999'
    @ ROW,41 SAY QTY4   PICTURE '@Z 99999'
    @ ROW,47 SAY LEFT(lcColoral,7)

    IF INVHDR.SPCINST = lcRpSpDz
      lnPrice    = PRICE * 12     
      lnTotQty   = TOTQTY / 12
      lnSubTotal = lnPrice * lnTotQty
      lnpict     = "PICTURE '999.99'"
    ELSE
      lnSubTotal =  PRICE * TOTQTY
      lnpict     = "PICTURE '999999'"
    ENDIF

    IF MOD(lnTotQty,1) = 0
      lnTotQty=INT(lnTotQty)
      lnpict = IIF(INVHDR.SPCINST = lcRpSpDz , "PICTURE '999.99'","PICTURE '999999'")
    ENDIF

    @ ROW,50 SAY IIF(INVHDR.SPCINST = lcRpSpDz,lnTotQty,TOTQTY) &lnpict
    @ ROW,62 SAY IIF(INVHDR.SPCINST = lcRpSpDz,lnPrice,PRICE)  PICTURE '9999.99'
    @ ROW,70 SAY lnSubTotal PICTURE '99999.99'

    DO CASE
      CASE INVHDR.SPCINST = lcRpSpDz
        @ ROW,82 SAY 'DOZ'
      CASE INVHDR.SPCINST = lcRpSpEh
        @ ROW,82 SAY 'EAC'
    ENDCASE

    lnTOTAL    = lnTOTAL + lnSubTotal
    ROW = ROW + 2

    * GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' .AND. .NOT. llnOTELOOP
    =lfGetcont()
     LOOP
  ENDIF

  *-- Print invoice Notepad.
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
      ROW = ROW + 1
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          llnOTELOOP = .T.
          lnNotLine = lnNotLine - 1
        ELSE
          llnOTELOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !llnOTELOOP
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
  @ ROW,70 SAY lnTOTAL  PICTURE '9999999.99'
  ROW=ROW+1

  IF DISCOUNT<>0
     @ ROW,12 SAY 'TOTAL - D I S C O U N T'
     @ ROW,70 SAY DISCOUNT PICTURE '9999999.99'
     ROW=ROW+1
  ENDIF

  IF XTAX .AND. XTAX_METH = 'M'
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW,12 SAY XTAX_DESC
    @ ROW,58 SAY XSTRING_RATE + ' %'
    @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    ROW = ROW + 1
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
     @ ROW,70 SAY WKAMT           PICTURE '9999999.99'
     ROW=ROW+1
  ENDIF

  IF XTAX .AND. XTAX_METH = 'A'
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW,12 SAY XTAX_DESC
    @ ROW,58 SAY XSTRING_RATE + ' %'
    @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    ROW = ROW + 1
  ENDIF

  *-- Print order Notepad.
  IF SEEK( 'B' + OrdHdr.ORDER , 'NotePad' )
    =lfPrnIns( 'B', OrdHdr.ORDER, 3, 16, 51-6, 65 )
  ENDIF

  @ 51,05 SAY InvHdr.ShipDate
  @ 51,25 SAY CARTONS           PICTURE '@Z 9999'
  @ 51,40 SAY WEIGHT            PICTURE '@Z 99999'
  @ 51,70 SAY INVHDR.TotalChg PICTURE '9999999.99'

  IF INVHDR.APPROVAL <>' ' .AND. UPPER(APPROVAL) <> 'DEC'
     @ 59,50 SAY 'APPROVAL: ' + INVHDR.APPROVAL
  ENDIF

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
*! Name      : lfPrnIns
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/31/2001
*! Purpose   : Print the notepad of the order.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrnIns()
*!*************************************************************
FUNCTION lfPrnIns
PARAMETERS lcType, lcCode,  lnNoOfLn, lnCol, lnRow, lnSize

PRIVATE lcType, lcCode, lnNoOfLn, lnCol, lnRow, lnSize ,lnAlias, lnCountr

lnSize   = IIF( lnSize > 75, 75, lnSize )
lnCountr = 1
lnAlias  = SELECT()

lnNotLine = 1
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 60
lnMemLn = MEMLINES(NOTEPAD.MNOTES)

SELECT NotePad
DO WHILE lnNotLine <= lnMemLn .AND.  lnCountr <= lnNoOfLn
  @ lnRow,lnCol SAY MLINE(MNOTES,lnNotLine)
  lnRow    = lnRow    + 1
  lnCountr = lnCountr + 1
  lnNotLine = lnNotLine + 1
ENDDO
SET MEMOWIDTH TO lnOldMemW

SELECT ( lnAlias )

*--End of lfPrnIns.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/31/2001
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
@ 11,15 SAY XBTNAME
IF CURLOOP = '1'
  @ 11,52 SAY XSTNAME
ENDIF

@ 12,15 SAY XBTADDR1
IF CURLOOP = '1'
  @ 12,52 SAY XSTADDR1
ENDIF

@ 13,15 SAY XBTADDR2
IF CURLOOP = '1'
  @ 13,52 SAY XSTADDR2
ENDIF

@ 14,15 SAY XBTADDR3
IF CURLOOP = '1'
  @ 14,52 SAY XSTADDR3
ENDIF

@ 18,01 SAY LEFT(CUSTPO,10)
@ 18,13 SAY DEPT
@ 18,19 SAY REP1
@ 18,23 SAY REP2
@ 18,28 SAY SUBSTR(IIF(CURLOOP='1' AND SEEK('S'+XACCOUNT+XSTORE,'CUSTOMER'),CUSTOMER.STNAME ,SPACE(30)),1,26)

SELECT INVHDR
@ 18,55 SAY IIF(CURLOOP='1',XSTORE,'')
@ 18,64 SAY ORDER
@ 18,72 SAY INVHDR.PIKTKT
@ 18,79 SAY XINVOICE
@ 20,19 SAY LEFT(lcTerms,14)
@ 20,58 SAY LEFT(lcShipVia,15)
@ 20,75 SAY InvHdr.ShipDate

*------------------
* PRINT SCALES
*------------------
SELE SCALE
ROW = 24
lcY   = ' '
SEEK 'S'+ INVLINE.SCALE

@ ROW,23 SAY PADL(ALLT(SZ1),5)+lcY+PADL(ALLT(SZ2),5)+lcY+PADL(ALLT(SZ3),5);
              +lcY+PADL(ALLT(SZ4),5)

ROW = 26

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/31/2001
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
SELECT INVHDR
@ 54,01 SAY XPHONE + ' ' + INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,75 SAY '******.**'

*--End of lfGetcont.
*!*************************************************************
*! Name      : LFSpInst
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/31/2001
*! Purpose   : Restore the value of the variables.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFSpInst()
*!*************************************************************
FUNCTION LFSpInst
PARAMETERS lcDummy

lcRpFile = 'ARPINVAL'
IF FILE(gcDataDir+lcRpFile+'.MEM')
  RESTORE FROM gcDataDir+lcRpFile+'.MEM' ADDITIVE
  lcEach = lcRpSpEh
  lcDozn = lcRpSpDz
ENDIF

*B804057,1 BWA 18/04/2001 Remove all from the code.[START]
IF TYPE('lcRpSpEh') = "U" AND TYPE('lcRpSpDz') = "U"
  DIMENSION laRpSpMan[1,1]
  STORE '' TO laRpSpMan

  SELECT CODES
  SELECT DISTINCT cDiscRep + '-' + cCode_NO FROM CODES WHERE cDefCode+cFld_Name = 'N'+'SPCINST   ' AND crltField = 'N';
                                          INTO ARRAY laRpSpMan

  DIMENSION laRpTarget[ALEN(laRpSpMan,1),1] , laRpSpcnst[ALEN(laRpSpMan,1),1]
  STORE '' TO laRpTarget , laRpSpcnst

  FOR lnLoop = 1 TO ALEN(laRpSpMan,1)
    laRpTarget[lnLoop,1] = ALLTRIM(STR(lnLoop))
  ENDFOR

  FOR lnFill = 1 TO ALEN(laRpSpMan,1)
    laRpSpcnst[lnFill,1] = LEFT(laRpSpMan[lnFill,1],30)
  ENDFOR
ENDIF
*B804057,1 [END]

RETURN ''
*--End of LFSpInst.