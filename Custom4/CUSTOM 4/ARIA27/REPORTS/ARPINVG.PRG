*:************************************************************************
*: Program file  : ARPINVD.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications : 
*:                 HDM B801884,1 Incorrect Bill to and ship to addresses
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
XCOUNT = 0
RELE ALL LIKE M_*
NEWDOC = .T.
MAXROW = 47
ROW    = 0
llNote = llRpInvNot


IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
LINEUP = .F.
STORE TRIM(QCOMPANY)                                       TO HLINE1
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon                         TO HLINE5

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
MSG3 = lcRpMsg1
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR
*SET FILTER TO (lcRpExp)

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SCAN FOR &lcASExp
*   IF XMODE  = 'M' .OR. XMODE = 'Y'
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
      *XPHONE    = laCompAdd[5]
      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
      XCODTAG   = CODTAG
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
      *--HDM B801884,1 Incorrect Bill to and ship to addresses[end]
      *** TAK 04/06/94
      SELE ORDHDR
      SEEK XORDER
      *** END TAK ***
        *-- Added by TAK 05/16/94
        IF INVHDR->CONSOL = 'Y'       
          SELECT CONSINVH
          SEEK XINVOICE
          SELECT CUSTOMER
          SEEK 'S'+XACCOUNT+CONSINVH->STORE
        ENDIF
        *-- End TAK 05/16/94
        SELE CUSTOMER
        *B801604,1 ADEL (Begin) Seek the DC  address if there is one.
        IF !EMPTY(DIST_CTR)
          lcDist = DIST_CTR
          = SEEK('S'+XACCOUNT+lcDist)
        ENDIF
        *B801604,1 (End)

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
  
      *TERMS
      PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')

      *SHIPVIA
      PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

      *SPC INST.
      PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
      *ahmed
      *= gfRltFld(INVHDR.UPSZONE , @laZone , 'SHIPVIA')
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
      
      XZN = laZone[1,2]


      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = llPrnFact        &&   IIF(XINVFACTOR='Y', .T., .F.)
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

      DO WHILE PRINTFCTR
        SELECT SYCFACT
        SEEK INVHDR->CFACCODE
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
   *-----------------
   * START PRINT
   *-----------------
   IF XCOUNT = 22
     ROW     = 0
     XCOUNT  = 0
     TOPROW  = ROW
   ELSE
     XCOUNT  = XCOUNT + 1
     TOPROW  = ROW
   ENDIF

   ROW = TOPROW + 3
  IF XINVNAME = 'Y'
*    SELE CODE 
*    SEEK 'D' + INVHDR->DIVISION
*    HLINE1 = IIF(FOUND().AND. .NOT. EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
     @ ROW,05 SAY HLINE1
     SELE INVHDR
   ENDIF
   @ ROW,38 SAY INVHDR->INVDATE
   @ ROW,66 SAY 'INVOICE: '+XINVOICE

   * LINE 4
   ROW = TOPROW + 4
   IF XINVNAME='Y'
     @ ROW,05 SAY HLINE2
   ENDIF
   IF .NOT. EMPTY(XDUNS)
     *B500670,1 MFM 04/26/95 (Begin).
     *@ ROW,32 SAY 'DUNS: '+ TRIM(XDUNS)
     *B800755,1 SHA 09/17/96(Begin) Commented out
     *@ ROW,37 SAY 'DUNS: '+ TRIM(XDUNS)
     *B800755,1 SHA 09/17/96(End)
     *B500670,1 MFM 04/26/95 (End).
   ENDIF
   IF PRINTFCTR
      @ ROW,50 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 5
   ROW = TOPROW + 5
   IF XINVNAME='Y'
      @ ROW,05 SAY HLINE3
   ENDIF
   IF PRINTFCTR
      @ ROW,50 SAY XFNAME
   ENDIF

   * LINE 6
   ROW = TOPROW + 6
   IF XINVNAME='Y'
      @ ROW,05 SAY HLINE4
   ENDIF
   IF PRINTFCTR
      @ ROW,50 SAY XFADDR1
   ENDIF

   * LINE 7
   ROW = TOPROW + 7
   IF XINVNAME='Y'
     *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
     *@ ROW,05 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
     *@ ROW,05 SAY XPHONE SIZE 1,16
     *@ ROW,05 SAY HLINE5
     *B500772,1 HISH 06/29/95.  ( End )
   ENDIF
      IF PRINTFCTR
      @ ROW,50 SAY XFADDR2
   ENDIF
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

   * LINE 8
   ROW = TOPROW + 8
   *B800755,1 SHA 09/17/96(Begin) Added
   @ ROW,05 SAY 'DUNS: '+ TRIM(XDUNS)
   *B800755,1 SHA 09/17/96(End)
   IF PRINTFCTR
      @ ROW,50 SAY XFADDR3
   ENDIF
   ROW = TOPROW + 11
   @ ROW,10 SAY XBTNAME
   IF CURLOOP = '1'
      @ ROW,56 SAY XSTNAME
   ENDIF
   ROW = TOPROW + 12
   @ ROW,10 SAY XBTADDR1
   IF CURLOOP = '1'
      @ ROW,56 SAY XSTADDR1
   ENDIF

   ROW = TOPROW + 13
   @ ROW,10 SAY XBTADDR2
   IF CURLOOP = '1'
      @ ROW,56 SAY XSTADDR2
   ENDIF

   ROW = TOPROW + 14
   @ ROW,10 SAY XBTADDR3
   IF CURLOOP = '1'
      @ ROW,56 SAY XSTADDR3
   ENDIF

   ROW = TOPROW + 17
   @ ROW,01 SAY ACCOUNT
   @ ROW,11 SAY ORDER
   @ ROW,21 SAY PTERMS
   @ ROW,38 SAY SUBSTR(PSHIPVIA,1,14)
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ ROW,53 SAY IIF(CURLOOP='1',SUBSTR(XSTORE,1,5),'')
   @ ROW,53 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),SUBSTR(lcDist,1,5),SUBSTR(XSTORE,1,5)),'')
   *B801604,1 (End)

   @ ROW,59 SAY DEPT

   @ ROW,66 SAY REP1
   @ ROW,70 SAY REP2
   @ ROW,76 SAY IIF(INVHDR->STATUS='V','** VOID **',CUSTPO)

   *------------------
   * PRINT SCALES
   *------------------
   ROW = TOPROW + 19
   select invline
   tscale1 = " "
   tscale2 = " "
   IF .NOT. XNOTE_LOOP        
     trec1   = RECNO ()
     tscale1 = " "
     tscale2 = " "
     do while .T.
       if invoice<>xinvoice .or. (tscale1<>" " .and. tscale2<>" ")
         exit
       endif
       if tscale1=" "
         tscale1=scale
       endif
       if scale<>tscale1
         tscale2=scale
       endif
       skip
     enddo
   
     SELE INVLINE
     IF .NOT. XNOLINES
       goto trec1
     ENDIF
   ENDIF                                                
   SELECT SCALE
   trec1=recno()
   i=49
   Y=' '
   do while i<51
     ti=chr(i)
     SEEK 'S'+TSCALE&TI
     if .not. eof()
  *    @ ROW,30 SAY SCALE+Y+SZ1+Y+SZ2+Y+SZ3+Y+SZ4+Y+SZ5+Y+SZ6+Y+SZ7+Y+SZ8
       *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
       @ ROW,30 SAY SCALE+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94

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
   XSCALE =  ' '
   ROW    = TOPROW + 23

   *** compute the maximum row to print the lines in just one page.
   IF .NOT. XNOTE_LOOP 
     MAXROW = GETMAXROW('INVOICE=XINVOICE',ROW,(TOPROW+41),XTOT_LINES)
   ENDIF  
   DO WHILE CURLOOP = '1' .AND. .NOT. XNOTE_LOOP           
      SELECT INVLINE

      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW  
         EXIT
      ENDIF

      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF

      KEY = INVLINE->STYLE &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE

      @ ROW,01 SAY IIF(!EMPTY(ALTSTYLE),SUBSTR(ALTSTYLE,1,7),SUBSTR(STYLE,1,7)) 
*      @ ROW,08 SAY IIF(!EMPTY(ALTCOLOR),SUBSTR(ALTCOLOR,1,3),SUBSTR(COLOR,1,3))       &&TMI 06/16/94
      @ ROW,12 SAY SUBSTR(STYLE->DESC,1,15)
      @ ROW,30 SAY STYLE->SCALE
      
      *Reham On 08/13/96   ** Begin ** 
      *Reham On 08/13/96 Increase the picture to be 4 digits instead of 3.
      @ ROW,32 SAY QTY1   PICTURE '@Z 9999'
      @ ROW,36 SAY QTY2   PICTURE '@Z 9999'
      @ ROW,40 SAY QTY3   PICTURE '@Z 9999'
      @ ROW,44 SAY QTY4   PICTURE '@Z 9999'
      @ ROW,48 SAY QTY5   PICTURE '@Z 9999'
      @ ROW,52 SAY QTY6   PICTURE '@Z 9999'
      @ ROW,56 SAY QTY7   PICTURE '@Z 9999'
      @ ROW,60 SAY QTY8   PICTURE '@Z 9999'
      *Reham On 08/13/96   ** End  **
      @ ROW,63 SAY TOTQTY PICTURE '999999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,69  SAY PRICE     PICTURE '9999.99'
      @ ROW,77  SAY XLINETOT  PICTURE '999999.99'

      ROW = ROW + 1

      IF LINEUP
         EXIT
      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP

   ENDDO

   *** END LINE PROCESSING
  IF LINEUP .AND. .NOT. XNOTE_LOOP .AND. gcDevice = 'PRINTER'
     EJECT
       IF gfModalGen("QRM40140B40000","Dialog") = 1
         SET DEVICE TO PRINT
         LINEUP =.T.
       ELSE
         LINEUP =.F.
       ENDIF
     SET DEVICE TO PRINT
     STORE 0.00 TO XPIECES, XSUBTOTAL
  ENDIF

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. .NOT. XNOTE_LOOP                 
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      ROW = TOPROW + 45
      @ ROW,01 SAY INVHDR->PIKTKT
      @ ROW,08 SAY INVDATE
      @ ROW,17 SAY PSHIPVIA
      @ ROW,43 SAY '***'
      @ ROW,51 SAY '*****'
      @ ROW,73 SAY '******.**'
      ROW = TOPROW + 51
      LOOP
   ENDIF
IF llNote
   SELECT NOTEPAD
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75
   
   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   
   IF TYPE + KEY = 'C' + XINVOICE
     *MAXROW = GETMAXROW ("TYPE+KEY='C'+XINVOICE",ROW+1,(TOPROW+41),XTOT_LINES)
     MAXROW = IIF (lnMemLins >= (TOPROW+41) - (ROW+1), (TOPROW+41) - 1,;
                   IIF (lnMemLins >= ((TOPROW+41)-(ROW+1))-XTOT_LINES ,;
                   (ROW+1+lnMemLins)-1 , (TOPROW+41)-XTOT_LINES ))

     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
     DO WHILE lnNotLine <= lnMemLins

       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
         EXIT
       ELSE
         XNOTE_LOOP = .F.
         @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         ROW = ROW + 1
       ENDIF
       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF .NOT. XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       ROW = TOPROW + 45
       @ ROW,01 SAY INVHDR->PIKTKT
       @ ROW,08 SAY INVHDR->INVDATE
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
ENDIF
   SELECT INVHDR
   @ ROW,11 SAY '** MERCHANDISE **'
   @ ROW,50 SAY 'TOTAL PIECES'
   @ ROW,63 SAY XPIECES              PICTURE '999999'
   @ ROW,76 SAY XSUBTOTAL            PICTURE '9999999.99' 
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,11 SAY 'LESS : DISCOUNT'
      @ ROW,75 SAY DISCOUNT             PICTURE '99999999.99' 
      ROW=ROW+1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,76 SAY IIF(INVHDR->STATUS='V',INVHDR->VTAX_AMT,INVHDR->TAX_AMT) PICTURE '9999999.99'
     ROW = ROW + 1
     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,76 SAY IIF(InvHdr.Status='V',InvHdr.nvPSTAmt,InvHdr.nPSTAmt) PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,11 SAY 'FREIGHT'
      @ ROW,76 SAY WKAMT                PICTURE '9999999.99' 
      ROW=ROW+1
   ENDIF

   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     @ ROW,64 SAY XSTRING_RATE + '%'
     *-- End TMI 01/17/95
     @ ROW,76 SAY IIF(INVHDR->STATUS='V',INVHDR->VTAX_AMT,INVHDR->TAX_AMT)  PICTURE '9999999.99'
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,76 SAY IIF(InvHdr.Status='V',InvHdr.nvPSTAmt,InvHdr.nPSTAmt) PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   ROW = TOPROW + 41
   @ ROW,08 SAY XNOTE1
   ROW = ROW + 1
   @ ROW,08 SAY XNOTE2

   ROW = TOPROW + 43
   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ ROW,01 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF

   ROW = TOPROW + 45
   @ ROW,01 SAY INVHDR->PIKTKT
   @ ROW,08 SAY INVDATE
   @ ROW,17 SAY PSHIPVIA
   @ ROW,43 SAY CARTONS                  PICTURE '@Z 999'

   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ ROW,51 SAY WEIGHT                   PICTURE '@Z 99999'
   @ ROW,51 SAY WEIGHT                   PICTURE '@Z 99.99'
   *B607103,1 KHM 05/04/2003 (End)

   @ ROW,72 SAY INVHDR->TOTALCHG         PICTURE '9999999.99' 

   ROW = TOPROW + 48
   @ ROW,36 SAY 'INVOICE: '+INVHDR->INVOICE

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ ROW,10 SAY MSG1
      ROW = ROW + 1
      @ ROW,10 SAY MSG2
   ENDIF
   ROW = TOPROW + 51
   SELECT INVHDR
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
   *-- GET NEXT HEADER RECORD
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN

   ELSE
     NEWDOC = .T.
   ENDIF
*--HDM

ENDSCAN
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

