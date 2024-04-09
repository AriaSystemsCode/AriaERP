*!***************************************************************************
*! Program file  : ARPINVME.PRG
*! Program desc. : Customized Invoice for MEO10 (This form exactly like for K).
*! Date          : 06/17/2002
*! System        : Aria Advantage Series.
*! Module        : ACCOUNT RECEIVABLE (AR)
*! Developer     : Sameh Saiid Ezzat(SSE)
*! Tracking Job Number: C102726
*!***************************************************************************
*! Example : DO ARPINVME
*!***************************************************************************
*

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
XDUNS = gfGetMemVar('XDUNS',gcAct_Comp)

STORE 0 TO lnLenth , lnClrLen , lnClrPos
*--THE STYLE LENGTH
lnLenth = LEN(gfItemMask('PM'))

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

NEWDOC = .T.
MAXROW = 47
lcDist = ' '
IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

lnNotLine = 1

SELECT INVHDR

*------------------------------
* SECTION: MAIN LOOP
*------------------------------

*LINEUP = .F.
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]                  TO HLINE4
lnNotLine = 1
STORE .F. TO llNoRec
PRIVATE lnLoop , llPhonExst
llPhonExst = .F.
FOR lnLoop = 1 TO 4
  IF ALLTRIM(laCompAdd[lnLoop]) = ALLTRIM(TRANSFORM(lcCompPhon , lcPhonPict))
    llPhonExst = .T.
    HLINE5 = ''
  ENDIF
ENDFOR
IF !llPhonExst
  STORE TRANSFORM(lcCompPhon , lcPhonPict) TO HLINE5
ENDIF  

IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE HLINE5 TO HLINE4
  STORE ''     TO HLINE5
ENDIF

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

lcZone = ''
DECLARE laZone[1,2]

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg3

SET DEVICE TO PRINT
PRIVATE lcFirstInv

XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE
lcFirstInv = Invoice 
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

PRIVATE XINVOICE , lnDefLoop
lnDefLoop = 0
XINVOICE = ''

DO lpPrint

SET DEVICE TO SCREEN
RETURN
*-- End of program


*!***************************************************************************
*! Name      : lpPrint
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/23/2002
*! Purpose   : Print the report
*!***************************************************************************
*! Called from : ARPINVME.PRG
*!***************************************************************************
*
PROCEDURE lpPrint
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT INVHDR
SCAN FOR &lcASExp
  lnDefLoop = 1
  FOR lnDefLoop = 1 TO 3
    WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
    SELECT INVHDR
    XINVOICE = INVOICE

    *-----------------------------------------------------------------
    * Get invoice header, line items, and financial history records.
    * If any records are not found, skip to next invoice.
    * Initialize document totals.
    *-----------------------------------------------------------------
    IF NEWDOC
      PRTERR = 0
      STORE 0.00 TO XPIECES, XSUBTOTAL

      SELECT INVHDR
      SEEK XINVOICE
      *IF EOF()
      *  SELECT &INVHTEMP
      *  SKIP
      *  LOOP
      *ENDIF
      NEWDOC    = .F.
      XORDER    = ORDER
      XPHONE    = INVHDR.PHONE    
      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
      XORDER    = ORDER
      XPIKTKT   = PIKTKT
      XACCOUNT  = ACCOUNT
      XSTORE    = STORE
      XSEASON   = SEASON
      XDIVISION = CDIVISION

      *** GET THE BILL TO AND SHIP ADDRESS
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

      SELE CUSTOMER
      IF !EMPTY(DIST_CTR)
        lcDist = DIST_CTR
        =SEEK('S'+XACCOUNT+lcDist)
      ENDIF
      
      ****** FIND THE INVOICE LINES
      SELECT INVLINE
      SEEK XINVOICE
      IF EOF()
        PRTERR = 2
      ENDIF
     
      *** GET THE DESCRIPTION ABOUT THE CODES
      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      *TERMS
      PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')

      *SHIPVIA
      PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

      *SPC INST.
      PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')      
      XZN = laZone[1,2]

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = llPrnFact         && Comes from form settings file
      XFNAME  = lcFacName           && Factor company name.
      XFADDR1 = laFactor[1]         && Factor address#1.
      XFADDR2 = laFactor[2]
      XFADDR3 = laFactor[3]
      IF LEN(TRIM(laFactor[2])) = 0  && If address#2 is empty fill it with address#3.
        XFADDR2 = laFactor[3]
        XFADDR3 = ''
      ENDIF

      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
    ENDIF
    ****** END NEWDOC
    *-----------------
    * START PRINT
    *-----------------
    * LINE 2   
    SELECT INVHDR
    @ 02,40 SAY INVDATE
    @ 02,50 SAY 'INVOICE: '+XINVOICE
    @ 02,68 SAY 'PO: '+INVHDR->CUSTPO

    * LINE 3
    IF XINVNAME='Y'
      @ 03,05 SAY HLINE1
      SELE INVHDR
    ENDIF
    IF !EMPTY(XDUNS)
      @ 03,50 SAY 'DUNS: '+ ALLTRIM(XDUNS)
    ENDIF

    * LINE 4
    IF XINVNAME='Y'
      @ 04,05 SAY HLINE2
    ENDIF
   
    IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 04,50 SAY 'R E M I T   T O:'
    ENDIF

    * LINE 5
    IF XINVNAME='Y'
      @ 05,05 SAY HLINE3
    ENDIF

    IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 05,50 SAY XFNAME
    ENDIF

    * LINE 6
    IF XINVNAME='Y'
      @ 06,05 SAY HLINE4
    ENDIF

    IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 06,50 SAY XFADDR1
    ENDIF

    * LINE 7
    IF XINVNAME='Y'
      @ 07,05 SAY HLINE5
    ENDIF

    IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 07,50 SAY XFADDR2
    ENDIF

    * LINE 8
    IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 08,50 SAY XFADDR3
    ENDIF

    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    =lfSolSpAdr()
    SELECT CUSTOMER
    XBTNAME  = BTNAME
    XBTADDR1 = laSoldTo[1] &&CADDRESS1
    XBTADDR2 = laSoldTo[2] &&CADDRESS2
    XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF
    
    SELECT InvHdr
    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF

    * LINE 10
    @ 10,10 SAY XBTNAME
    IF CURLOOP = '1'
      @ 10,47 SAY XSTNAME
    ENDIF

    * LINE 11
    @ 11,10 SAY XBTADDR1
    IF CURLOOP = '1'
      @ 11,47 SAY XSTADDR1
    ENDIF

    * LINE 12
    @ 12,10 SAY XBTADDR2
    IF CURLOOP = '1'
      @ 12,47 SAY XSTADDR2
    ENDIF

    * LINE 13
    @ 13,10 SAY XBTADDR3
    IF CURLOOP = '1'
      @ 13,47 SAY XSTADDR3
    ENDIF

    @ 17,01 SAY ACCOUNT
    @ 17,09 SAY LEFT(CUSTPO,11)
    @ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
    @ 17,30 SAY DEPT
    @ 17,36 SAY ORDER
    @ 17,44 SAY LEFT(PTERMS,15)
    @ 17,60 SAY REP1
    @ 17,64 SAY REP2
    @ 17,68 SAY LEFT(PSHIPVIA,12)
   
    *------------------
    * PRINT SCALES
    *------------------
    lcScalStr = SPACE(0)
    lcInvNo   = InvHDR.Invoice
    DO lpGtScl WITH lcInvNo
    SELECT Scale
    Row = 19
    FOR lnI = 1 TO LEN( lcScalStr )
      lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
      SEEK 'S' + lcSizeScal
      @ Row,22 SAY lcSizeScal
      @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
      @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
      @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
      @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
      @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
      @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
      @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
      @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')
      Row = Row + 1
    ENDFOR

    *---------------------------------------------------------------
    * [1] LINE ITEM PRINT LOOP
    *---------------------------------------------------------------
    SELECT INVLINE
    XSTORE = STORE
    XSCALE =  ' '
    ROW    = 25
    DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
        IF ROW >= MAXROW
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
      KEY = INVLINE->STYLE        &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY
      SELECT INVLINE
      @ ROW,00 SAY LEFT(STYLE,lnLenth)
      @ ROW,14 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
      @ ROW,22 SAY LEFT(STYLE.SCALE,1)
      @ ROW,23 SAY QTY1   PICTURE '@Z 9999'
      @ ROW,27 SAY QTY2   PICTURE '@Z 9999'
      @ ROW,31 SAY QTY3   PICTURE '@Z 9999'
      @ ROW,35 SAY QTY4   PICTURE '@Z 9999'
      @ ROW,39 SAY QTY5   PICTURE '@Z 9999'
      @ ROW,43 SAY QTY6   PICTURE '@Z 9999'
      @ ROW,47 SAY QTY7   PICTURE '@Z 9999'
      @ ROW,51 SAY QTY8   PICTURE '@Z 9999'
      @ ROW,55 SAY TOTQTY PICTURE '99999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,63  SAY PRICE     PICTURE '9999.99'
      @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW + 1
      @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
      ROW = ROW + 1
      
      *IF LINEUP
      *  EXIT
      *ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
    ENDDO
    *** END LINE PROCESSING

    *IF LINEUP .AND. .NOT. XNOTE_LOOP .AND. gcDevice = 'PRINTER'
    *  EJECT
    *  IF gfModalGen("QRM40140B40000","Dialog") = 1
    *    SET DEVICE TO PRINT
    *    LINEUP =.T.
    *  ELSE
    *    LINEUP =.F.
    *  ENDIF
    *  SET DEVICE TO PRINT
    *  STORE 0.00 TO XPIECES, XSUBTOTAL
    *ENDIF

    *---------------------------------------------------------------
    * CURRENTLY PROCESSING REGULAR LINE ITEMS
    *---------------------------------------------------------------
    ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

    IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR      
      @ 55,01 SAY XPHONE SIZE 1,16
      @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2      
      IF LEN(TRIM(MSG1 + MSG2)) >0
        @ 56,10 SAY MSG1
        @ 57,10 SAY MSG2
      ENDIF
      @ 60,02 SAY INVHDR->PIKTKT
      @ 60,09 SAY ShipDate
      @ 60,19 SAY '***'
      @ 60,25 SAY '*****'
      @ 60,35 SAY XINVOICE
      @ 60,71 SAY '******.**'
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
        ROW = ROW + 1 
   
        DO WHILE lnNotLine <= lnMemLins
          IF ROW >= MAXROW
            XNOTE_LOOP = .T.
          ELSE
            XNOTE_LOOP = .F.
            @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)         
            ROW = ROW + 1
          ENDIF
          IF ROW >= MAXROW      && if there is a line will de delete from 
            =lfGetcont()        && the memo field
            =lfGetInHdr()
          ENDIF
          lnNotLine = lnNotLine + 1
        ENDDO
     
        IF !XNOTE_LOOP
          @ ROW,02 SAY '* -- END OF NOTES -- *'
          lnNotLine = 1
          ROW = ROW + 1 
        ELSE
          @ ROW+1,12 SAY 'C O N T I N U E D ...'
          SELECT INVHDR
          @ 55,01 SAY XPHONE SIZE 1,16
          @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
          IF LEN(TRIM(MSG1 + MSG2)) >0
            @ 56,10 SAY MSG1
            @ 57,10 SAY MSG2
          ENDIF
          @ 60,02 SAY INVHDR->PIKTKT
          @ 60,09 SAY InvHdr.ShipDate
          @ 60,19 SAY '***'
          @ 60,25 SAY '*****'
          @ 60,35 SAY XINVOICE
          @ 60,71 SAY '******.**'
          LOOP
        ENDIF
      ENDIF
      SET MEMOWIDTH TO lnOldMemW
    ENDIF

    SELECT INVHDR
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
    IF XTAX .AND. XTAX_METH = 'M' 
      XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
      @ ROW,12 SAY XTAX_DESC
      IF !EMPTY(lcTaxRefr)
        @ Row,33 SAY lcTaxRefr
      ENDIF  
      @ ROW,64 SAY XSTRING_RATE + '%'
      @ ROW,70 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
      ROW = ROW + 1
      IF InvHdr.nPSTAmt <> 0
        @ Row,12 SAY 'P S T   T A X'
        @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
        @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
        Row = Row + 1
      ENDIF  
      IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
        @ Row,12 SAY 'H S T   T A X'
        @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
        @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
        Row = Row + 1
      ENDIF  
    ENDIF

    WKAMT = FREIGHT + INSUR + COD
    IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,70 SAY WKAMT            PICTURE '9999999.99'
      ROW=ROW+1
    ENDIF

    *** Print the tax rate and tax amount 
    IF XTAX .AND. XTAX_METH = 'A'
      XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
      @ ROW,12 SAY XTAX_DESC
      IF !EMPTY(lcTaxRefr)
        @ Row,33 SAY lcTaxRefr
      ENDIF  
      @ ROW,64 SAY XSTRING_RATE + '%'
      @ ROW,70 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
      ROW = ROW + 1
      IF InvHdr.nPSTAmt <> 0
        @ Row,12 SAY 'P S T   T A X'
        @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
        @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
        Row = Row + 1
      ENDIF  
      IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
        @ Row,12 SAY 'H S T   T A X'
        @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
        @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
        Row = Row + 1
      ENDIF  
    ENDIF  
    @ 55,01 SAY XPHONE SIZE 1,16
    @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

    IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 56,10 SAY MSG1
      @ 57,10 SAY MSG2
    ENDIF

    IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 59,56 SAY 'APPROVAL: ' + INVHDR->APPROVAL
    ENDIF

    @ 60,00 SAY INVHDR->PIKTKT
    @ 60,08 SAY ShipDate
    @ 60,17 SAY CARTONS               PICTURE '@Z 999'
    @ 60,25 SAY WEIGHT                PICTURE '@Z 99999'
    @ 60,35 SAY INVHDR->INVOICE
    @ 60,70 SAY INVHDR->TOTALCHG      PICTURE '9999999.99'

    SELECT InvHdr 
    SEEK XINVOICE
    IF gcDevice <> 'SCREEN'
      INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
    ENDIF
     
    SELECT INVHDR
    IF !EOF()
      NEWDOC = .T.
    ENDIF
  ENDFOR
ENDSCAN

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lpPrint

*!*************************************************************
*! Name : lpGtScl.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Build a string holding the first uniqe 5 scales
*!            from the invoice line scal.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Passed :
*!        Parameters : 
*!           lcInvNo : The Current invoice number.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Returned : 
*!        Variables  : lcScalStr.
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

  IF !( ALLTRIM(Scale) $ lcScalStr )
    lcScalStr = lcScalStr + ALLTRIM(Scale)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp,"InvLine")

*-- Select the old alias.
SELECT (lcAlias)

*-- EOF( lpGtScl )

*!*************************************************************
*! Name : gfGetZone.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

IF !USED('FRTZONES')
  lnOldWrk = SELECT()
  SELECT 0
  DO NETUSE WITH '&QLB.FRTZONES','&QLB.FRTZONES','SH'
  SELECT (lnOldWrk)
ENDIF

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'FRTZONES'),'',FRTZONES.ZONE)

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
SELECT INVHDR
       
@ 55,01 SAY XPHONE SIZE 1,16
@ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
       
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,02 SAY INVHDR->PIKTKT
@ 60,09 SAY InvHdr.ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,71 SAY '******.**'

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 08/02/2000
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

* LINE 2
SELECT INVHDR
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+INVHDR->CUSTPO

* LINE 3
IF XINVNAME='Y'
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF
IF !EMPTY(XDUNS)
  @ 03,50 SAY 'DUNS: '+ ALLTRIM(XDUNS)
ENDIF

* LINE 4
IF XINVNAME='Y'
  @ 04,05 SAY HLINE2
ENDIF
   
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY HLINE3
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF XINVNAME='Y'
  @ 06,05 SAY HLINE4
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
IF XINVNAME='Y'
  @ 07,05 SAY HLINE5
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 08,50 SAY XFADDR3
ENDIF
SELECT CUSTOMER
SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
=lfSolSpAdr()
SELECT CUSTOMER
XBTNAME  = BTNAME
XBTADDR1 = laSoldTo[1] &&CADDRESS1
XBTADDR2 = laSoldTo[2] &&CADDRESS2
XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(XBTADDR2)) =0
  XBTADDR2 = XBTADDR3
  XBTADDR3 = ''
ENDIF
SELE ORDHDR

XSTADDR1 = laShipTo[1] &&CADDRESS12
XSTADDR2 = laShipTo[2] &&laShipTo22
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(XSTADDR2)) =0
  XSTADDR2 = XSTADDR3
  XSTADDR3 = ''
ENDIF

* LINE 10
@ 10,10 SAY XBTNAME
IF CURLOOP = '1'
  @ 10,47 SAY XSTNAME
ENDIF

* LINE 11
@ 11,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 11,47 SAY XSTADDR1
ENDIF

* LINE 12
@ 12,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 12,47 SAY XSTADDR2
ENDIF

* LINE 13
@ 13,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ 13,47 SAY XSTADDR3
ENDIF

@ 17,01 SAY ACCOUNT
@ 17,09 SAY LEFT(CUSTPO,11)
@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY LEFT(PTERMS,15)
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,68 SAY LEFT(PSHIPVIA,12)

*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)
lcInvNo   = InvHDR.Invoice
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 19
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  @ Row,22 SAY lcSizeScal

  @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
  @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
  @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
  @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
  @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
  @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
  @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
  @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')

  Row = Row + 1
ENDFOR

Row = 25          && NEW LINE in the function
