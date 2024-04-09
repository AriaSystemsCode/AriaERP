*:************************************************************************
*: Program file  : ARPINVJ.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications:
*:       HDM B801884,1 Incorrect Bill to and ship to addresses
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B803289,1 NAD 05/29/2000 Fix bug the form  prints the first page of the invoice only and stops  
*:B803289,1                if the invoice was more than one page.
*:B803915,1 BWA 12/26/2000 1) Fix the bug of the color and its descreption didn't print.
*:B803915,1                2) Fix the bug of not change the flag when printing the invoice.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XNOTE_LOOP = .F.
XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

NEWDOC = .T.
ROW    = 0
XCOUNT = 0
lcColorDes = ""
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
*B801604,1 (End)

lnNotLine = 1

SELECT INVHDR

CLEAR TYPEAHEAD
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
lcZone = ''
DECLARE laZone[1,2]
lcDist = ' '
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon                         TO HLINE5

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg1
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

*B803915,1 BWA 12/26/2000 Fix the bug of the color and its descreption didn't print.[START]
*-- Get the STYLE and COLOR length
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos , lnLenth
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lnLenth  = LEN(gfItemMask('PM'))

IF lnLenth # 7 AND lnClrLen # 3
  =gfModalGen('INM40175B40011','ALERT')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*B803915,1 [END]

SELECT INVHDR
SCAN FOR &lcASExp
  *-- Get Customer DUNS[start]
  lnPrevAl = SELECT()
  SELECT CUSTOMER
  XDUNS = DUNS
  SELECT (lnPrevAl)
  *-- Get Customer DUNS[end]

  SELECT INVHDR
  XINVOICE = INVOICE
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------
    IF NEWDOC
      XNOLINES = .F.
      STORE 0.00 TO XPIECES, XSUBTOTAL
      SELECT INVHDR
      SEEK XINVOICE
      *** This variable will hold the number of lines to be
      *** used to print the totals (MERCHANDISE,DISCOUNT,TAX,FREIGHT).
      XTOT_LINES = 1
      IF XTAX  
        XTOT_LINES = XTOT_LINES + 1
      ENDIF
      IF DISCOUNT <> 0
        XTOT_LINES = XTOT_LINES + 1 
      ENDIF
      IF FREIGHT + INSUR + COD <> 0
        XTOT_LINES = XTOT_LINES + 1 
      ENDIF

      NEWDOC    = .F.
      XORDER    = ORDER
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

*--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
    XBTNAME  = BTNAME
    XBTADDR1 = CADDRESS1
    XBTADDR2 = CADDRESS2
    XBTADDR3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5

      IF LEN(TRIM(XBTADDR2)) =0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF
      *** TAK 04/06/94
      SELE ORDHDR
      SEEK XORDER
      IF ALT_SHPTO
        XSTADDR1 = CADDRESS12
        XSTADDR2 = laShipTo22
        XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
*--HDM B801884,1 Incorrect Bill to and ship to addresses[end]
      *** END TAK ***
        *-- Added by TAK 05/16/94
        IF INVHDR.CONSOL = 'Y'       
          SELECT CONSINVH
          SEEK XINVOICE
          SELECT CUSTOMER
          SEEK 'S'+XACCOUNT+CONSINVH.STORE
        ENDIF
        *-- End TAK 05/16/94
        SELE CUSTOMER
        *B801604,1 ADEL (Begin) Seek the DC  address if there is one.
        IF !EMPTY(DIST_CTR)
          lcDist = DIST_CTR
          = SEEK('S'+XACCOUNT+lcDist)
        ENDIF
        *B801604,1 (End)
        XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
        XSTADDR1 = CADDRESS12
        XSTADDR2 = CADDRESS22
        XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ENDIF
      ***
      ****** FIND THE INVOICE LINES
      ***
      SELECT INVLINE
      SEEK XINVOICE
      IF EOF()
         XNOLINES=.T.
      ENDIF
      *** GET THE DESCRIPTION ABOUT THE CODES

      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      PTERMS = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
      PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
      PSPCINST= gfCodDes(INVHDR.SPCINST,'SPCINST')
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
      
      XZN = laZone[1,2]
      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
*      PRINTFCTR = IIF(XINVFACTOR='Y', .T., .F.)
      PRINTFCTR = llPrnFact
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

      DO WHILE PRINTFCTR
         SELECT SYCFACT
         SEEK INVHDR.CFACCODE
         DO CASE
           CASE EOF()
             STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
           CASE FOUND()
             XFNAME  = CFACCOMP
             XFADDR1 = CADDRESS1
             XFADDR2 = CADDRESS2
             XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
             IF CADDRESS2 = ' '
                CADDRESS2 = CADDRESS3
                CADDRESS3 = ' '
             ENDIF
         ENDCASE
         EXIT
      ENDDO

      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ENDIF

   SELECT INVHDR
   IF XCOUNT = 22
     ROW     = 0
     XCOUNT  = 0
     TOPROW  = ROW
   ELSE
     XCOUNT  = XCOUNT + 1
     TOPROW  = ROW
   ENDIF

   * LINE 1
   ROW = TOPROW + 1
   IF llPrntComp
     @ ROW,02 SAY HLINE1
     SELE INVHDR
   ENDIF
   IF llPrntComp
     @ ROW,02 SAY HLINE2
   ENDIF
   IF .NOT. EMPTY(XDUNS)
     *B500670,1 MFM 04/26/95 (Begin).
     *@ ROW,29 SAY 'DUNS: '+ TRIM(XDUNS)
     @ ROW,34 SAY 'DUNS: '+ TRIM(XDUNS)
     *B500670,1 MFM 04/26/95 (End).
   ENDIF
   IF PRINTFCTR
      *B801179,1 SHA 07/15/97(Begin)Move the remit to address 2 char. to the right.
      @ ROW,60 SAY 'R E M I T   T O:'  &&@ ROW,58 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 2
   ROW = TOPROW + 2
   IF llPrntComp
      @ ROW,02 SAY HLINE3
   ENDIF
   IF PRINTFCTR
      @ ROW,60 SAY XFNAME  &&@ ROW,58 SAY XFNAME
   ENDIF

   * LINE 3
   ROW = TOPROW + 3
   IF llPrntComp
      @ ROW,02 SAY HLINE4
   ENDIF
   IF PRINTFCTR
      @ ROW,60 SAY XFADDR1  &&@ ROW,58 SAY XFADDR1
   ENDIF

   * LINE 4
   ROW = TOPROW + 4
   IF llPrntComp
      @ ROW,02 SAY HLINE5
   ENDIF
   IF PRINTFCTR
      @ ROW,60 SAY XFADDR2  &&@ ROW,58 SAY XFADDR2
   ENDIF

   * LINE 5
   ROW = TOPROW + 5
   IF PRINTFCTR
      @ ROW,60 SAY XFADDR3  &&@ ROW,58 SAY XFADDR3
   ENDIF
   *B801179,1 SHA 07/15/97(End)
   * LINE 7
   ROW = TOPROW + 7
   @ ROW,33 SAY 'INVOICE: '+XINVOICE
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
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
    *** TAK 04/06/94
    SELE ORDHDR
    SEEK XORDER
    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]

   * LINE 10
   ROW = TOPROW + 9
   @ ROW,10 SAY XBTNAME
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTNAME
   ENDIF
   ROW = TOPROW + 10
   @ ROW,10 SAY XBTADDR1
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR1
   ENDIF

   ROW = TOPROW + 11
   @ ROW,10 SAY XBTADDR2
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR2
   ENDIF

   ROW = TOPROW + 12
   @ ROW,10 SAY XBTADDR3
   IF CURLOOP = '1'
      @ ROW,58 SAY XSTADDR3
   ENDIF

   ROW = TOPROW + 15
   @ ROW,01 SAY ACCOUNT
   @ ROW,11 SAY ORDER
   @ ROW,21 SAY PTERMS
   @ ROW,37 SAY PSHIPVIA
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ ROW,53 SAY IIF(CURLOOP='1',XSTORE,'')
   @ ROW,53 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   *B801604,1 (End)
    @ ROW,61 SAY DEPT
   @ ROW,66 SAY REP1
   @ ROW,70 SAY REP2
   @ ROW,76 SAY IIF(INVHDR.STATUS='V','** VOID **',CUSTPO)

   ROW = TOPROW + 17
   select invline
      
   IF .NOT. XNOTE_LOOP       
     trec1   = RECNO()
     tscale1 = " "
     tscale2 = " "
     tscale3 = " "
     do while .T.
       if invoice<>xinvoice .or. (tscale1<>" " .and. tscale2<>" " .and. tscale3<>" ")
         exit
       endif
       if tscale1=" "
         tscale1=scale
       endif
       if scale<>tscale1 .and. tscale2=" "
         tscale2=scale
       endif
       if scale<>tscale1 .and. scale<>tscale2
         tscale3=scale
       endif
       skip
     enddo
   
     
     SELE INVLINE

     IF .NOT. XNOLINES     
       goto trec1
     ENDIF
   ENDIF

   SELECT SCALE
   trec1 = recno()
   i = 49
   Y = ' '
   do while i < 52
     ti = chr (i)
     SEEK 'S'+TSCALE&TI
     if .not. eof ()
*      @ ROW,34 SAY SZ1+Y+SZ2+Y+SZ3+Y+SZ4+Y+SZ5+Y+SZ6+Y+SZ7+Y+SZ8
      *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
      *B801179,1 SHA 07/15/97(Begin)Move the size scales one char. to the left.
      *@ ROW,34 SAY PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
      @ ROW,33 SAY PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                    PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
      *B801179,1 SHA 07/15/97(End)
      *B500670,1 MFM 04/26/95 (End).
       row=row+1
     endif
     i=i+1
   enddo
   goto trec1
   
   CNT = 1

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE = ' '
   ROW    = TOPROW + 21

   *** To compute the maximum row to print the lines in just one page.
   IF .NOT. XNOTE_LOOP 
     MAXROW = GETMAXROW('INVOICE=XINVOICE',ROW,(TOPROW+39),XTOT_LINES)
   ENDIF  

   DO WHILE CURLOOP = '1' .AND. .NOT. XNOTE_LOOP           
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW 
        *B803289,1  (Start) If new page then Print it's header
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
          LOOP
        ENDIF
        *B803289,1 (End)
        EXIT
      ENDIF

      IF TOTQTY = 0
        SKIP
        LOOP
      ENDIF

      KEY = INVLINE.STYLE         &&+ INVLINE.COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE

      @ ROW,00 SAY SUBSTR(STYLE,1,7)

      *B803915,1 BWA 12/26/2000 Fix the bug of the color and its descreption didn't print.[START]
      *@ ROW,08 SAY SUBSTR(COLOR,1,3)                  &&TMI 06/16/94
       @ ROW,08 SAY SUBSTR(STYLE,lnClrPos,3)
      *B803915,1 [END]
      @ ROW,12 SAY SUBSTR(STYLE.DESC,1,13)

      *B803915,1 BWA 12/26/2000 Fix the bug of the color and its descreption didn't print.[START]
      *@ ROW,25 SAY SUBSTR(ALLTRIM(XCOLDESC),1,8)
      lcColorDes = gfCodDes(SUBSTR(INVLINE.STYLE,lnClrPos,lnClrLen),'COLOR')
      @ ROW,26 SAY SUBSTR(ALLTRIM(lcColorDes),1,6)
      *B803915,1 [END]

      *B801179,1 SHA 07/15/97(Begin)Move the quantities one char. to the left.
      *B801452,1 SHA 01/16/97(Begin)Print 4 digits fo each size break down.
      @ ROW,33 SAY QTY1   PICTURE '@Z 9999'  &&@ ROW,34 SAY QTY1   PICTURE '@Z 999'
      @ ROW,37 SAY QTY2   PICTURE '@Z 9999'  &&@ ROW,38 SAY QTY2   PICTURE '@Z 999'
      @ ROW,41 SAY QTY3   PICTURE '@Z 9999'  &&@ ROW,42 SAY QTY3   PICTURE '@Z 999'
      @ ROW,45 SAY QTY4   PICTURE '@Z 9999'  &&@ ROW,46 SAY QTY4   PICTURE '@Z 999'
      @ ROW,49 SAY QTY5   PICTURE '@Z 9999'  &&@ ROW,50 SAY QTY5   PICTURE '@Z 999'
      @ ROW,53 SAY QTY6   PICTURE '@Z 9999'  &&@ ROW,54 SAY QTY6   PICTURE '@Z 999'
      @ ROW,57 SAY QTY7   PICTURE '@Z 9999'  &&@ ROW,58 SAY QTY7   PICTURE '@Z 999'
      @ ROW,61 SAY QTY8   PICTURE '@Z 9999'  &&@ ROW,62 SAY QTY8   PICTURE '@Z 999'
      @ ROW,65 SAY TOTQTY PICTURE '99999'   &&@ ROW,66 SAY TOTQTY PICTURE '99999'
      *B801452,1 SHA 01/16/97(End)
      *B801179,1 SHA 07/15/97(End)
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,71  SAY PRICE     PICTURE '9999.99'
      @ ROW,79  SAY XLINETOT  PICTURE '999999.99'

      ROW = ROW + 1

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. .NOT. XNOTE_LOOP                 
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      ROW = TOPROW + 43
      @ ROW,01 SAY INVHDR.PIKTKT
      @ ROW,08 SAY INVDATE
      @ ROW,17 SAY PSHIPVIA
      @ ROW,43 SAY '***'
      @ ROW,51 SAY '*****'
      @ ROW,73 SAY '******.**'
      ROW = TOPROW + 51
      LOOP
   ENDIF

   SELECT NOTEPAD
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75
   
   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     *B803289,1 (Start) Get the notepad in both cases
     *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   *B803289,1  (End)
   IF TYPE + KEY = 'C' + XINVOICE
     *MAXROW = GETMAXROW ("TYPE+KEY='C'+XINVOICE",ROW+1,(TOPROW+39),XTOT_LINES)
     MAXROW = IIF (lnMemLins >= (TOPROW+39) - (ROW+1), (TOPROW+39) - 1,;
                   IIF (lnMemLins >= ((TOPROW+39)-(ROW+1))-XTOT_LINES ,;
                   (ROW+1+lnMemLins)-1 , (TOPROW+39)-XTOT_LINES ))
     
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 
     
     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
      DO WHILE lnNotLine <= lnMemLins

       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
         *B803289,1 (Start) Commented out 
         *EXIT
         *B803289,1 (End) 
       ELSE
         XNOTE_LOOP = .F.
         *B803289,1  (Start) add the name of the file (NotPad)
         *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
         *B803289,1 (End)
         ROW = ROW + 1
       ENDIF
       *B803289,1 (Start) Print the header for the new page
       IF ROW >= MAXROW      && if there is a line will de delete from 
         =lfGetcont()        && the memo field
         =lfGetInHdr()
       ENDIF
       *B803289,1 (End)
       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF .NOT. XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       ROW = TOPROW + 43
       @ ROW,01 SAY INVHDR.PIKTKT
       @ ROW,08 SAY INVHDR.INVDATE
       @ ROW,17 SAY PSHIPVIA
       @ ROW,43 SAY '***'
       @ ROW,51 SAY '*****'
       @ ROW,73 SAY '******.**'
       ROW = TOPROW + 51
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).

   SELECT INVHDR
   @ ROW,11 SAY '** MERCHANDISE **'
   @ ROW,50 SAY 'TOTAL PIECES'
   *B801179,1 SHA 07/15/97(Begin)Move the total pieces one char. to the left.
   @ ROW,65 SAY XPIECES   PICTURE '99999'  &&@ ROW,66 SAY XPIECES   PICTURE '99999'
   *B801179,1 SHA 07/15/97(End)
   @ ROW,78 SAY XSUBTOTAL PICTURE '9999999.99'     
   ROW = ROW + 1

   IF DISCOUNT <> 0
      @ ROW,11 SAY 'LESS : DISCOUNT'
      @ ROW,77 SAY DISCOUNT PICTURE '99999999.99'   
      ROW = ROW + 1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,78 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1
     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,11 SAY 'FREIGHT'
      @ ROW,78 SAY WKAMT PICTURE '9999999.99'       
      ROW = ROW + 1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,78 SAY INVHDR.TAX_AMT PICTURE '9999999.99'
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,78 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   ROW = TOPROW + 39
   @ ROW,08 SAY XNOTE1
   ROW = ROW + 1
   @ ROW,08 SAY XNOTE2

   ROW = TOPROW + 41
   IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ ROW,01 SAY 'APPROVAL: ' + INVHDR.APPROVAL
   ENDIF
   @ ROW,63 SAY 'INVOICE: '+INVHDR.INVOICE

   ROW = TOPROW + 43
   @ ROW,01 SAY INVHDR.PIKTKT
   @ ROW,08 SAY INVDATE
   @ ROW,17 SAY PSHIPVIA
   @ ROW,43 SAY CARTONS                  PICTURE '@Z 999'

   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ ROW,51 SAY WEIGHT                   PICTURE '@Z 99999'
   @ ROW,51 SAY WEIGHT                   PICTURE '@Z 99.99'
   *B607103,1 KHM 05/04/2003 (End)
   
   @ ROW,72 SAY INVHDR.TOTALCHG          PICTURE '9999999.99' 

   ROW = TOPROW + 51
   SELECT INVHDR

   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF

   *B803915,1 BWA 12/26/2000 Fix the bug of not change the flag when printing the invoice.[START]
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
   *B803915,1 [END]

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

***********************************************************************
* FUNC : GETMAXROW.
* DESC : Function to compute the MAXROW for printing the invoice lines
*        and the invoice Notepad. 
* AUTH : YASSER MOHAMMED ALY (YMA).
* DATE : 02/22/94.
* NOTE : CALLED FROM INV810I.
* PARM : XCONDITION --> We will count the lines according to this condition.
*        XSTART ------> The start row of printing.
*        XEND --------> The last row of printing.
*        XTOT_LINES --> Number of rows to print the totals.
***********************************************************************
FUNCTION GETMAXROW
PARAMETERS XCONDITION,XSTART,XEND,XTOT_LINES

XREC_NO = RECNO()
COUNT REST TO XLINES_NO WHILE &XCONDITION
GOTO XREC_NO
MAXROW = IIF (XLINES_NO >= XEND-XSTART, XEND - 1        ,;
              IIF (XLINES_NO >= (XEND-XSTART)-XTOT_LINES ,;
                  (XSTART+XLINES_NO)-1 , XEND-XTOT_LINES ))
RETURN (MAXROW)

*!*************************************************************
*! Name : gfGetZone.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! Date : 04/05/95.
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
*! Name      : lfGetInHdr
*! Developer : Nader Anis (NAD) 
*! Date      : 05/29/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*! Ref       : B803289,1 
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
* LINE 1
ROW = TOPROW + 1
IF llPrntComp
  @ ROW,02 SAY HLINE1
  SELECT INVHDR
ENDIF
IF llPrntComp
  @ ROW,02 SAY HLINE2
ENDIF
IF .NOT. EMPTY(XDUNS)     
  @ ROW,34 SAY 'DUNS: '+ TRIM(XDUNS)    
ENDIF
IF PRINTFCTR
  @ ROW,60 SAY 'R E M I T   T O:'  &&@ ROW,58 SAY 'R E M I T   T O:'
ENDIF

* LINE 2
ROW = TOPROW + 2
IF llPrntComp
  @ ROW,02 SAY HLINE3
ENDIF
IF PRINTFCTR
  @ ROW,60 SAY XFNAME  &&@ ROW,58 SAY XFNAME
ENDIF

* LINE 3
ROW = TOPROW + 3
IF llPrntComp
  @ ROW,02 SAY HLINE4
ENDIF
IF PRINTFCTR
  @ ROW,60 SAY XFADDR1  &&@ ROW,58 SAY XFADDR1
ENDIF

* LINE 4
ROW = TOPROW + 4
IF llPrntComp
  @ ROW,02 SAY HLINE5
ENDIF
IF PRINTFCTR
  @ ROW,60 SAY XFADDR2  &&@ ROW,58 SAY XFADDR2
ENDIF

* LINE 5
ROW = TOPROW + 5
IF PRINTFCTR
  @ ROW,60 SAY XFADDR3  &&@ ROW,58 SAY XFADDR3
ENDIF
* LINE 7
ROW = TOPROW + 7
@ ROW,33 SAY 'INVOICE: '+XINVOICE
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
SEEK XORDER
XSTADDR1 = laShipTo[1] &&CADDRESS12
XSTADDR2 = laShipTo[2] &&laShipTo22
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  IF LEN(TRIM(XSTADDR2)) =0
    XSTADDR2 = XSTADDR3
    XSTADDR3 = ''
  ENDIF    

* LINE 10
ROW = TOPROW + 9
@ ROW,10 SAY XBTNAME
IF CURLOOP = '1'
  @ ROW,58 SAY XSTNAME
ENDIF
ROW = TOPROW + 10
@ ROW,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR1
ENDIF

ROW = TOPROW + 11
@ ROW,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR2
ENDIF

ROW = TOPROW + 12
@ ROW,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR3
ENDIF

ROW = TOPROW + 15
@ ROW,01 SAY ACCOUNT
@ ROW,11 SAY ORDER
@ ROW,21 SAY PTERMS
@ ROW,37 SAY PSHIPVIA
@ ROW,53 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ ROW,61 SAY DEPT
@ ROW,66 SAY REP1
@ ROW,70 SAY REP2
@ ROW,76 SAY IIF(INVHDR.STATUS='V','** VOID **',CUSTPO)
ROW = TOPROW + 17
SELECT invline
if !eof()
  IF .NOT. XNOTE_LOOP        
    trec1   = RECNO()
    tscale1 = " "
    tscale2 = " "
    tscale3 = " "
    DO WHILE .T.
      IF invoice<>xinvoice .OR. (tscale1<>" " .AND. tscale2<>" " .AND. tscale3<>" ")
        EXIT
      ENDIF
      IF tscale1=" "
        tscale1=scale
      ENDIF
      IF scale<>tscale1 .AND. tscale2=" "
        tscale2=scale
      ENDIF
      IF scale<>tscale1 .AND. scale<>tscale2
        tscale3=scale
      ENDIF
      SKIP
    ENDDO
  
    SELECT INVLINE
    IF .NOT. XNOLINES
      GOTO trec1
    ENDIF
  ENDIF                                      

  SELECT SCALE
  trec1 = RECNO()
  i = 49
  Y = ' '
  DO WHILE i < 52
    ti = chr (i)
    SEEK 'S'+TSCALE&TI
    IF ! EOF()      
      @ ROW,33 SAY PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                   PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                   PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                   PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')  
      
      row=row+1
    ENDIF
    i=i+1
  ENDDO
  GOTO trec1
  CNT = 1
  ROW  = TOPROW + 21
ENDIF
*!*************************************************************
*! Name      : lfGetcont
*! Developer : Nader Anis 
*! Date      : 05/28/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*! Ref       : B803289,1
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
ROW = TOPROW + 43
@ ROW,01 SAY INVHDR.PIKTKT
@ ROW,08 SAY INVHDR.INVDATE
@ ROW,17 SAY PSHIPVIA
@ ROW,43 SAY '***'
@ ROW,51 SAY '*****'
@ ROW,73 SAY '******.**'
ROW = TOPROW + 51
