*:************************************************************************
*: Program file  : ARPINVC.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:Modifications  :
*:                HDM B801884,1 Incorrect Bill to and ship to addresses
*B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*                         it does not print the 2nd page and also does not print any totals.
*B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*C102212,1 ADEL 03/25/2001 Add HST tax to all Invoice formats.
*B804563,1 SSE 12/26/2001 Fix the differences between this form in A27 & A26.
*B606045,1 NAD 05/29/2002 Variable XDuns not found . 
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XNOTE_LOOP = .F.   && Flag to indicate whether we have finished
                   && printing the Notepad or not.

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

*B804563,1 Get color position and length. [Begin]
PRIVATE lnMajLen , lnClrPos
STORE 0 TO lnMajLen , lnNonMajLn
lnMajLen = LEN(gfItemMask('PM'))
lnNonMajLn = LEN(gfItemMask('PN'))

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

gcPhnFrmt = lcPhonPict
llNote = llRpInvNot
NEWDOC = .T.
MAXROW = 47

*B801604,1 (Bebin) Initilize the distribution center var.

*B804563,1 Commented out. [Begin]
*lcDist = ' '
*B804563,1 Commented out. [End]

*B801604,1 (End)

STORE TRIM(QCOMPANY)                                       TO HLINE1
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3

*B804563,1 Remove the phone number from the address line.. [Begin]
*STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE TRIM(laCompAdd[3])+' '+ALLTRIM(laCompAdd[4]) TO HLINE4
*B804563,1 Remove the phone number from the address line.. [End]

lnNotLine = 1
STORE .F. TO llNoRec

*B804563,1 Remove the company fax number. [Begin]
*STORE lcCompPhon TO HLINE5
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
*B804563,1 Remove the company fax number. [End]

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

lnNotLine = 1

SELECT INVHDR

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
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

*DO WHILE INKEY() <>32
SCAN FOR &lcASExp

    XINVOICE = INVOICE

    *B804563,1 Commented out. [Begin]
    *-- Get Customer DUNS[start]
    *lnPrevAl = SELECT()
    *SELECT CUSTOMER
    *XDUNS = DUNS
    *SELECT (lnPrevAl)
    *-- Get Customer DUNS[end]
    *B804563,1 Commented out. [End]

   *-----------------------------------------------------------------
   * Get invoice header, line items, and financial history records.
   * If any records are not found, skip to next invoice.
   * Initialize document totals.
   *-----------------------------------------------------------------
   IF NEWDOC
      PRTERR = 0
      STORE 0.00 TO XPIECES, XSUBTOTAL
      SELECT INVHDR
*      SEEK XINVOICE
      NEWDOC    = .F.
      XORDER    = ORDER
*      XPHONE    = IIF(EMPTY(PHONE),'',PHONE)

      *XPHONE    = laCompAdd[5]
      
      *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
      *                         it does not print the 2nd page and also does not print any totals.[START]
      XPHONE    = INVHDR.PHONE    
      *B803023,1 BWA 08/02/2000 [END]
      
      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
      XORDER    = ORDER
      XPIKTKT   = PIKTKT
      XACCOUNT  = ACCOUNT
      XSTORE    = STORE
      XSEASON   = SEASON
      XDIVISION = CDIVISION
      ***
      *** GET THE BILL TO AND SHIP ADDRESS
      ***
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

      *B804563,1 Seek starting with type 'O'. [Begin] 
      *SEEK XORDER
      SEEK 'O' + XORDER
      *B804563,1 Seek starting with type 'O'. [End] 
      
      *** END TAK ***
        *-- Added by TAK 05/16/94
        IF INVHDR->CONSOL = 'Y'       
          SELECT CONSINVH
          SEEK XINVOICE
          SELECT CUSTOMER
          SEEK 'S'+XACCOUNT+CONSINVH->STORE
        ENDIF
        *-- End TAK 05/16/94
        
        *B804563,1 Commented out. [Begin]
        *SELE CUSTOMER
        *B801604,1 ADEL (Begin) Seek the DC  address if there is one.
        *IF !EMPTY(DIST_CTR)
        *  lcDist = DIST_CTR
        *  = SEEK('S'+XACCOUNT+lcDist)
        *ENDIF
        *B801604,1 (End)
        *B804563,1 Commented out. [End]
      
      ***
      ****** FIND THE INVOICE LINES
      ***
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
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
      
      *B804563,1 Get the print factor flag. [Begin]
      PRINTFCTR = llPrnFact
      *B804563,1 Get the print factor flag. [End]
      
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
   ***
   ****** END NEWDOC
   ***
   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   SELECT INVHDR
   @ 02,40 SAY INVDATE
   @ 02,50 SAY 'INVOICE: '+XINVOICE

   * LINE 3
   IF XINVNAME='Y'
*     SELE CODE 
*     SEEK 'D' + INVHDR->DIVISION
*     HLINE1 = IIF(FOUND().AND. .NOT. EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
     
     *B804563,1 Print division long name instead of division description. [Begin]
     *HLINE1 = IIF(!EMPTY(lcDivLName), gfCodDes(INVHDR->CDIVISION,'CDIVISION') , QCOMPANY)
     HLINE1 = IIF(!EMPTY(lcDivLName), lcDivLName , QCOMPANY)
     *B804563,1 Print division long name instead of division description. [End]

     @ 03,05 SAY HLINE1
     SELE INVHDR
   ENDIF

   * LINE 4
   IF XINVNAME='Y'
     @ 04,05 SAY HLINE2
   ENDIF
   
   *B804563,1 Print Company duns no. [Begin]
   *IF .NOT. EMPTY(XDUNS)
   *B500670,1 MFM 04/26/95 (Begin).
     *@ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
   *B500670,1 MFM 04/26/95 (End).
   IF !EMPTY(lcDunsNo)
     @ 04,37 SAY 'DUNS: '+ ALLTRIM(lcDunsNo)
   *B804563,1 Print Company duns no. [Begin]

   ENDIF
   
   *IF PRINTFCTR
   IF PRINTFCTR AND !EMPTY(INVHDR.cFACCODE)
      @ 04,50 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 5
   IF XINVNAME='Y'
      @ 05,05 SAY HLINE3
   ENDIF
   IF PRINTFCTR
      @ 05,50 SAY XFNAME
   ENDIF

   * LINE 6
   IF XINVNAME='Y'
      @ 06,05 SAY HLINE4
   ENDIF
   IF PRINTFCTR
      @ 06,50 SAY XFADDR1
   ENDIF

   * LINE 7
   IF XINVNAME='Y'
     *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
     @ 07,05 SAY HLINE5  PICTURE gcPhnFrmt SIZE 1,16
     *@ 07,05 SAY HLINE5
     *B500772,1 HISH 06/29/95.  ( End )
   ENDIF
   IF PRINTFCTR
      @ 07,50 SAY XFADDR2
   ENDIF

   *--HDM B801884,1[START]
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    =lfSolSpAdr()
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
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

    *B804563,1 Seek starting with type 'O'. [Begin] 
    *SEEK XORDER
    SEEK 'O' + XORDER
    *B804563,1 Seek starting with type 'O'. [End] 

    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]



   * LINE 8
   IF PRINTFCTR
      @ 08,50 SAY XFADDR3
   ENDIF

   @ 11,08 SAY XBTNAME
   IF CURLOOP = '1'
      @ 11,55 SAY XSTNAME
   ENDIF

   @ 12,08 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 12,55 SAY XSTADDR1
   ENDIF

   @ 13,08 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 13,55 SAY XSTADDR2
   ENDIF

   @ 14,08 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 14,55 SAY XSTADDR3
   ENDIF

   @ 17,01 SAY ACCOUNT
   @ 17,09 SAY ORDER

   *B804563,1 Print only 15 characters for both Terms and ShipVia. [Begin]
   *@ 17,20 SAY PTERMS
   *@ 17,36 SAY PSHIPVIA
   @ 17,20 SAY LEFT(PTERMS,15)
   @ 17,36 SAY LEFT(PSHIPVIA,15)
   *B804563,1 Print only 15 characters for both Terms and ShipVia. [End]

   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 17,52 SAY IIF(CURLOOP='1',XSTORE,'')
   
   *B804563,1 Print the store only. [Begin]
   *@ 17,52 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   @ 17,52 SAY IIF(CURLOOP='1',XSTORE,'')
   *B804563,1 Print the store only. [End]
   
      
  
   *B801604,1 (End)
   @ 17,60 SAY DEPT
   @ 17,65 SAY REP1
   @ 17,69 SAY REP2
   @ 17,73 SAY INVHDR->CUSTPO

   *------------------
   * PRINT SCALES
   *------------------
   SELE SCALE
   SEEK 'S'
   ROW = 19
   X   = 1
   Y   = ' '
   DO WHILE X <= 5 .AND. .NOT. EOF() .AND. TYPE='S'
     *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
     *NAD
     *@ ROW,29 SAY SCALE+Y+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
     
     
     @ ROW,29 SAY SCALE+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
     *NAD
     *B500670,1 MFM 04/26/95 (End).
     *B500670,1 MFM 04/26/95 (End).
     
     
     ROW = ROW + 1
     X  = X + 1
     SKIP
   ENDDO

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 24

   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW>=47
      
        *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
        *                         it does not print the 2nd page and also does not print any totals.[START]
        IF ROW >= 47
           =lfGetcont()
           =lfGetInHdr()
           LOOP
         ENDIF
        *B803023,1 BWA 08/02/2000 [END]         

        EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE->STYLE       && + INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE
      
      *B804563,1 print the color again. [Begin]
      *nad
      *@ ROW,00 SAY SUBSTR(STYLE,1,7)
      *@ ROW,08 SAY SUBSTR(COLOR,1,3)              &&TMI 06/16/94
      *@ ROW,12 SAY SUBSTR(STYLE->DESC,1,17)
      *@ ROW,00 SAY SUBSTR(Style,1,lnMajLen)
      *@ ROW,12 SAY SUBSTR(Style,lnMajLen+2,lnNonMajLn)
      *@ ROW,19 SAY SUBSTR(Style.Desc,1,9)
      @ ROW,00 SAY SUBSTR(STYLE,1,7)
      @ ROW,08 SAY SUBSTR(Style,lnMajLen+2,3)              &&TMI 06/16/94
      @ ROW,12 SAY SUBSTR(STYLE->DESC,1,17)
      *nad
      *B804563,1 print the color again. [End]

      @ ROW,29 SAY STYLE->SCALE
      @ ROW,32 SAY QTY1   PICTURE '@Z 999'
      @ ROW,36 SAY QTY2   PICTURE '@Z 999'
      @ ROW,40 SAY QTY3   PICTURE '@Z 999'
      @ ROW,44 SAY QTY4   PICTURE '@Z 999'
      @ ROW,48 SAY QTY5   PICTURE '@Z 999'
      @ ROW,52 SAY QTY6   PICTURE '@Z 999'
      @ ROW,56 SAY QTY7   PICTURE '@Z 999'
      @ ROW,60 SAY QTY8   PICTURE '@Z 999'
      @ ROW,64 SAY TOTQTY PICTURE '9999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,69  SAY PRICE     PICTURE '9999.99'
      @ ROW,77  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW+1
*      IF LINEUP
*         EXIT
*      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT
*   IF LINEUP .AND. !XNOTE_LOOP
*      EJECT
*      DO MSG22 WITH ' ANOTHER LINEUP? Y/N/Q => ','YNQ'
*      DO CASE
*         CASE CHOICE ='Q'
*           RETURN
*         CASE CHOICE ='N'
*           LINEUP =.F.
*      ENDCASE
*      DO MSG22 WITH 'PRINTING INVOICES - <Space Bar> TO ABORT','@'
*      SET DEVICE TO PRINT
*      STORE 0.00 TO XPIECES, XSUBTOTAL
*      LOOP
*   ENDIF

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR

      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
*      @ 54,05 SAY XPHONE  PICTURE +gcPhnFrmt SIZE 1,16
      @ 54,05 SAY XPHONE  SIZE 1,16
      @ 54,23 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *@ 54,05 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *B500772,1 HISH 06/29/95.  ( End )

      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
      ENDIF
      @ 60,01 SAY INVHDR->PIKTKT
      @ 60,09 SAY INVDATE
      @ 60,42 SAY '****'
      @ 60,48 SAY '*****'
      @ 60,77 SAY '******.**'
      LOOP
   ENDIF

   *** Print invoice Notepad.
 IF llNote
   SELECT NOTEPAD
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75

   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     *B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.[START]
     *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF

   lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   *B603444,1 BWA 10/02/2000 [END]
     
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

      *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
      DO WHILE lnNotLine <= lnMemLins
       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
         
         *B803023,1 BWA 08/02/2000 [START]         
         *EXIT        && I comment this line of code.
         *B803023,1 BWA 08/02/2000 [END]
         
       ELSE
         XNOTE_LOOP = .F.
         
         *B803023,1 BWA 08/02/2000 [START]
         *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
         *B803023,1 BWA 08/02/2000 [END]
                  
         ROW = ROW + 1
       ENDIF
       
       *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
       *                         it does not print the 2nd page and also does not print any totals.[START]
       IF ROW >= MAXROW      && if there is a line will de delete from 
         =lfGetcont()        && the memo field
         =lfGetInHdr()
       ENDIF
       *B803023,1 BWA 08/02/2000 [END]

       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF .NOT. XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       SELECT INVHDR
       
       *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
*       @ 54,05 SAY XPHONE  PICTURE gcPhnFrmt SIZE 1,16
       @ 54,05 SAY XPHONE SIZE 1,16
       @ 54,23 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
       *@ 54,05 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
       *B500772,1 HISH 06/29/95.  ( End )

       IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
       ENDIF
       @ 60,01 SAY INVHDR->PIKTKT
       @ 60,09 SAY INVHDR->INVDATE
       @ 60,42 SAY '****'
       @ 60,48 SAY '*****'
       @ 60,77 SAY '******.**'
       LOOP
     ENDIF
   ENDIF
   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
 ENDIF

   SELECT INVHDR
   ROW = ROW + 1
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,62 SAY XPIECES    PICTURE '999999'
   @ ROW,76 SAY XSUBTOTAL  PICTURE '9999999.99'
   ROW = ROW + 1
   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,75 SAY DISCOUNT  PICTURE '99999999.99'
      ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,76 SAY INVHDR->TAX_AMT  
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,76 SAY WKAMT  PICTURE '9999999.99'
      ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/10/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/10/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,76 SAY INVHDR->TAX_AMT  
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,76 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
*   @ 54,05 SAY XPHONE  PICTURE gcPhnFrmt SIZE 1,16
   @ 54,05 SAY XPHONE  SIZE 1,16
   @ 54,23 SAY XNOTE1 + ' ' + XNOTE2
   *@ 54,05 SAY XPHONE + ' ' + XNOTE1 + ' ' + XNOTE2
   *B500772,1 HISH 06/29/95.  ( End )

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 56,10 SAY MSG1
      @ 57,10 SAY MSG2
   ENDIF

   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 58,60 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF

   @ 60,01 SAY INVHDR->PIKTKT
   @ 60,09 SAY INVDATE
   @ 60,42 SAY CARTONS           PICTURE '@Z 999'
   
   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ 60,48 SAY WEIGHT            PICTURE '@Z 99999'
   @ 60,48 SAY WEIGHT            PICTURE '@Z 99.99'
   *B607103,1 KHM 05/04/2003 (End)
   
   @ 60,76 SAY INVHDR->TOTALCHG  PICTURE '9999999.99'
*   IF XMODE  = 'N'
*      EXIT
*   ENDIF
   SELECT INVHDR
*   SEEK XINVOICE

   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
*--HDM
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF
*--HDM

   *B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
   *B803585,1 [END]

   * GET NEXT HEADER RECORD
*   SELECT &INVHTEMP
*   SKIP
*   NEWDOC = .T.
ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810B.PRG
*--------------------------------
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

*B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*                         it does not print the 2nd page and also does not print any totals.[START]
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
@ 54,05 SAY XPHONE SIZE 1,16
@ 54,23 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,01 SAY INVHDR->PIKTKT
@ 60,09 SAY INVHDR->INVDATE
@ 60,42 SAY '****'
@ 60,48 SAY '*****'
@ 60,77 SAY '******.**'

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

* LINE 3
IF XINVNAME='Y'
  HLINE1 = IIF(!EMPTY(lcDivLName), gfCodDes(INVHDR->CDIVISION,'CDIVISION') , QCOMPANY)
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF

* LINE 4
IF XINVNAME='Y'
  @ 04,05 SAY HLINE2
ENDIF
*B606045,1 Variable Xduns not found (Start)
*IF .NOT. EMPTY(XDUNS)
*@ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
IF .NOT. EMPTY(lcDunsNo)
  @ 04,37 SAY 'DUNS: '+ TRIM(lcDunsNo)
*B606045,1 Variable Xduns not found (End)
  ENDIF
IF PRINTFCTR
  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY HLINE3
ENDIF
IF PRINTFCTR
  @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF XINVNAME='Y'
  @ 06,05 SAY HLINE4
ENDIF
IF PRINTFCTR
  @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
IF XINVNAME='Y'
  @ 07,05 SAY HLINE5  PICTURE gcPhnFrmt SIZE 1,16
ENDIF
IF PRINTFCTR
  @ 07,50 SAY XFADDR2
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

*B804563,1 Seek starting with type 'O'. [Begin] 
*SEEK XORDER
SEEK 'O' + XORDER
*B804563,1 Seek starting with type 'O'. [End] 

XSTADDR1 = laShipTo[1] &&CADDRESS12
XSTADDR2 = laShipTo[2] &&laShipTo22
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(XSTADDR2)) =0
  XSTADDR2 = XSTADDR3
  XSTADDR3 = ''
ENDIF

* LINE 8
IF PRINTFCTR
  @ 08,50 SAY XFADDR3
ENDIF

@ 11,08 SAY XBTNAME
IF CURLOOP = '1'
  @ 11,55 SAY XSTNAME
ENDIF

@ 12,08 SAY XBTADDR1
IF CURLOOP = '1'
  @ 12,55 SAY XSTADDR1
ENDIF

@ 13,08 SAY XBTADDR2
IF CURLOOP = '1'
  @ 13,55 SAY XSTADDR2
ENDIF

@ 14,08 SAY XBTADDR3
IF CURLOOP = '1'
  @ 14,55 SAY XSTADDR3
ENDIF

@ 17,01 SAY ACCOUNT
@ 17,09 SAY ORDER

*B606045,1 NAD 05/29/2002
*@ 17,20 SAY PTERMS
*@ 17,36 SAY PSHIPVIA
 @ 17,20 SAY LEFT(PTERMS,15)
 @ 17,36 SAY LEFT(PSHIPVIA,15)
*B606045,1 NAD 05/29/2002

*B804563,1 Print the store only. [Begin]
*@ 17,52 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 17,52 SAY IIF(CURLOOP='1',XSTORE,'')
*B804563,1 Print the store only. [End]

@ 17,60 SAY DEPT
@ 17,65 SAY REP1
@ 17,69 SAY REP2
@ 17,73 SAY INVHDR->CUSTPO

*------------------
* PRINT SCALES
*------------------
SELE SCALE
SEEK 'S'
ROW = 19
X   = 1
Y   = ' '
DO WHILE X <= 5 .AND. .NOT. EOF() .AND. TYPE='S'
  *nad
  *@ ROW,29 SAY SCALE+Y+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
  
  @ ROW,29 SAY SCALE+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                            PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
 
 
  *nad
  ROW = ROW + 1
  X  = X + 1
  SKIP
ENDDO

ROW = 24
*B803023,1 BWA 08/02/2000 [END]