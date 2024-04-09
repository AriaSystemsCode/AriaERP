*:***************************************************************************
*: Program file  : ARPINVFR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR FREE COUNTRY.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C#101247)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES 
*! Date          : 02/12/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102170
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVFR
*:***************************************************************************
*B605673,1 NAD 03/19/2002   Some alignments to the invoice form 

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
NEWDOC     = .T.
MAXROW     = 47
llIsCanada = IIF(gcContcode = 'CANADA', .T., .F.)

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
    STORE 0.00 TO XPIECES,XSUBTOTAL
    STORE SPACE(1) TO lcScale1 , lcScale2
    NEWDOC    = .F.
    XPHONE    = INVHDR.PHONE 
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XDIVISION = CDIVISION

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

    *-- GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    lcDvsion  = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
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

    lcShipVia = IIF(TRIM(lcShipVia) = "N/A" , "" ,TRIM(lcShipVia)) +XZN
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
  ROW    = 27

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
    @ ROW,00 SAY LEFT(STYLE,12)
    @ ROW,12 SAY SUBSTR(STYLE,lnClrPosCd,6)
    @ ROW,19 SAY LEFT(gfCodDes(SUBSTR(INVLINE.STYLE,lnClrPosCd,6),'COLOR'),9)
    @ ROW,29 SAY LEFT(STYLE.SCALE,1)
    @ ROW,31 SAY QTY1   PICTURE '@Z 999'
    @ ROW,35 SAY QTY2   PICTURE '@Z 999'
    @ ROW,39 SAY QTY3   PICTURE '@Z 999'
    @ ROW,43 SAY QTY4   PICTURE '@Z 999'      
    @ ROW,47 SAY QTY5   PICTURE '@Z 999'
    @ ROW,51 SAY QTY6   PICTURE '@Z 999'
    @ ROW,55 SAY QTY7   PICTURE '@Z 999'
    @ ROW,59 SAY QTY8   PICTURE '@Z 999'
    *B605673,1 NAD  
    *@ ROW,63 SAY TOTQTY PICTURE '9999'
    @ ROW,61 SAY TOTQTY PICTURE '9999'
    *B605673,1 NAD 
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    *B605673,1 NAD 
    *@ ROW,66  SAY PRICE     PICTURE '9999.99'
    @ ROW,64  SAY PRICE     PICTURE '9999.99'
    *B605673,1 NAD 
    @ ROW,74  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW+1

    DO lpPrtSku

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
      ROW = ROW + 1 
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
          lnNotLine = lnNotLine - 1
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

  @ 50,07 SAY XPHONE SIZE 1,16

  SELECT INVHDR
  @ 50,55 SAY 'MERCHANDISE'
  @ 50,73 SAY XSUBTOTAL            PICTURE '9999999.99'  

  ROW=51
  @ 51,07 SAY INVHDR.NOTE1

  IF XTAX .AND. XTAX_METH = 'M' 
    IF !EMPTY(lcTaxRefr)
      @ Row,55 SAY lcTaxRefr
      Row = Row + 1
    ENDIF  
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ ROW ,73 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF  

  IF ROW = 53
    @ 53,07 SAY INVHDR.NOTE2
    llNotePrnt = .T.
  ELSE
    llNotePrnt = .F.  
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
     @ ROW,55 SAY 'TOTAL - FREIGHT'
     @ ROW,73 SAY WKAMT       PICTURE '9999999.99'  
     ROW=ROW+1
  ENDIF

  IF DISCOUNT<>0
     @ ROW,55 SAY 'DISCOUNT'
     @ ROW,72 SAY DISCOUNT    PICTURE  '99999999.99'   
  ENDIF

  IF !llNotePrnt
    @ 53,07 SAY INVHDR.NOTE2
  ENDIF

  IF XTAX .AND. XTAX_METH = 'A'
    @ 54,55 SAY lcTaxRefr
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ 55 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ 55 ,73 SAY INVHDR.TAX_AMT   PICT '9999999.99'
  ENDIF  

  IF xTAx .AND. llIsCanada 
    lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))
    lnRow = IIF (xTAx_Meth='A',56,55)
    @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
    @ lnRow ,73 SAY InvHdr.nPstAmt PICTURE '9999999.99'
  ENDIF
  *B605673,1 NAD 
  *@ 58,0 SAY INVHDR.CARTONS    PICTURE '@Z 99999'
  *@ 58,6 SAY INVHDR.WEIGHT     PICTURE '@Z 99999'
  *@ 58,64 SAY XPIECES          PICTURE '9999999'
  *@ 58,73 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  
  @ 56,0 SAY INVHDR.CARTONS    PICTURE '@Z 99999'
  @ 56,6 SAY INVHDR.WEIGHT     PICTURE '@Z 99999'
  @ 56,64 SAY XPIECES          PICTURE '9999999'
  @ 56,73 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
  *B605673,1 NAD 
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
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/12/2001
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVF.PRG
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
*! Name      : lpPrtSku.
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/12/2001
*! Purpose   : Print the style/color Skus for a specific account.
*!*************************************************************
*! Called from : ARPINVFR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
  laSku = SPACE(16)
  SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    @ ROW,00 SAY lcStrToPrn
    ROW = ROW + 1
  ENDDO
ELSE
  @ ROW,00 SAY Sku
  @ ROW,16 SAY 'CUSTOMER SKU #'
  ROW=ROW+1
ENDIF
RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/12/2001
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
@ 01,60 SAY XINVOICE
@ 01,71 SAY INVDATE

IF llPrntComp
  @ 03,05 SAY HLINE1
ENDIF

@ 03,71 SAY SUBSTR( lcDvsion, 1, 14 )

*--LINE 4
IF llPrntComp
  @ 04,05 SAY HLINE2
ENDIF

*--LINE 5
IF llPrntComp
   @ 05,05 SAY HLINE3
ENDIF
@ 05,71 SAY ORDER

*--LINE 6
IF llPrntComp
  @ 06,05 SAY HLINE4
ENDIF

*--LINE 7
IF llPrntComp
  @ 07,05 SAY TRANSFORM(lcCompPhon , lcPhonPict)
ENDIF

@ 07,71 SAY PIKTKT
@ 09,71 SAY APPROVAL
@ 13,08 SAY XBTNAME
@ 13,54 SAY XSTNAME
@ 14,08 SAY XBTADDR1
@ 14,54 SAY XSTADDR1
@ 15,08 SAY XBTADDR2
@ 15,54 SAY XSTADDR2
@ 16,08 SAY XBTADDR3
@ 16,54 SAY XSTADDR3
@ 21,02 SAY ACCOUNT
@ 21,12 SAY LEFT(CUSTPO,10)
@ 21,23 SAY IIF(!EMPTY(CUSTOMER.DIST_CTR),CUSTOMER.DIST_CTR,STORE)
@ 21,33 SAY DEPT
@ 21,42 SAY LEFT(lcTerms,14)
@ 21,62 SAY REP1
@ 21,66 SAY REP2
@ 21,71 SAY SUBSTR(lcShipVia,1,14)

ROW = 23
SELECT INVLINE
IF !XNOTE_LOOP
  lcRec1 = RECNO()
    DO WHILE .T.
    IF INVOICE <> XINVOICE .OR. (lcScale1 <> " " .AND. lcScale2 <> " ")
      EXIT
    ENDIF
    IF lcScale1 = " "
      lcScale1 = SCALE
    ENDIF
    IF SCALE <> lcScale1
      lcScale2 = SCALE
    ENDIF
    SKIP
  ENDDO
  GOTO lcRec1
ENDIF                                                 
  
SELECT SCALE
lcRec1 = RECNO()
I = 49
Y = ' '
DO WHILE I < 51
  TI = CHR(I)
  SEEK 'S'+lcScale&TI
  IF !EOF()
    @ ROW,29 SAY LEFT(SCALE,1)+Y+PADL(SZ1,3,' ') + Y + PADL(SZ2,3,' ') + Y +;
                                 PADL(SZ3,3,' ') + Y + PADL(SZ4,3,' ') + Y +;
                                 PADL(SZ5,3,' ') + Y + PADL(SZ6,3,' ') + Y +;
                                 PADL(SZ7,3,' ') + Y + PADL(SZ8,3,' ')
    ROW = ROW + 1
  ENDIF
  I = I + 1
ENDDO
GOTO lcRec1
ROW    = 27

*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/12/2001
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

ROW = 50
@ 50,07 SAY XPHONE SIZE 1,16
@ 50,55 SAY 'MERCHANDISE'
@ 50,77 SAY '******'
ROW=ROW+1
@ 51,07 SAY INVHDR.NOTE1
@ 53,07 SAY INVHDR.NOTE2
@ 58,18 SAY INVHDR.CARTONS PICTURE '@Z 999'
@ 58,62 SAY '******'
@ 58,75 SAY '******'

*-- End of lfGetcont.