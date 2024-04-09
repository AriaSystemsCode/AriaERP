*:************************************************************************
*: Program file  : ARPINVE.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c)
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications:
*:                HDM B801884,1 Incorrect Bill to and ship to addresses
*:B602962,1 BWA 02/06/1999 Adjust invoice form H due to the attached form.
*:                         and print the right scales and all the memo lines  
*:B803023,1 BWA 02/08/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B803127,1 MHM 06/26/2000 Print ship date instead of enter date.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
&& printing the Notepad or not.

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
MAXROW = 47
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
*B801604,1 (End)

lnNotLine = 1
llNote = llRpInvNot
SELECT INVHDR

lcZone = ''
DECLARE laZone[1,2]

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg3
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict

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



*------------------------------
* SECTION: MAIN LOOP
*------------------------------
CLEAR TYPEAHEAD
lcZone = ''
DECLARE laZone[1,2]

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg3
XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict

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


SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
*--Begin Main Loop{HDM}
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

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
    PRTERR = 0
    STORE 0.00 TO XPIECES, XSUBTOTAL
    SELECT INVHDR
    SEEK XINVOICE
    NEWDOC = .F.
    XORDER = ORDER
    
    *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
    *                         it does not print the 2nd page and also does not print any totals. [START]
    *XPHONE    = laCompAdd[5]
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
      PRTERR = 2
    ENDIF

    *** GET THE DESCRIPTION ABOUT THE CODES [HDM]
    SELECT CODES
    SET ORDER TO CODES IN CODES

    *TERMS
    PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')

    *SHIPVIA
    PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

    *SPC. INST.
    PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
    *= gfRltFld(INVHDR.UPSZONE , @laZone , 'SHIPVIA')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')

    XZN = laZone[1,2]

    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
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
  ***
  ****** END NEWDOC
  ***
  *-----------------
  * START PRINT
  *-----------------
  * LINE 2
*B602962,1 BWA Collect all the header lines in a function to call this function from any where from the program
*SELECT INVHDR
*B602962,1 BWA  Modify the field dimension due to the attached form.
*@ 02,40 SAY INVDATE
*@ 02,50 SAY 'INVOICE: '+XINVOICE
*@ 02,68 SAY 'PO: '+INVHDR->CUSTPO
*@ 02,31 SAY INVDATE
*@ 02,41 SAY 'INVOICE: '+XINVOICE
*@ 02,59 SAY 'PO: '+INVHDR->CUSTPO
*B602962,1 BWA  [END]
* LINE 3
*IF XINVNAME='Y'
  *     SELE CODE
  *     SEEK 'D' + INVHDR->DIVISION
  *     HLINE1 = IIF(FOUND() .AND. .NOT. EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
*  @ 03,05 SAY HLINE1
*  SELE INVHDR
*ENDIF

  * LINE 4
*  IF XINVNAME='Y'
*    @ 04,05 SAY HLINE2
*  ENDIF
*  IF .NOT. EMPTY(XDUNS)
    *B500670,1 MFM 04/26/95 (Begin).
    *@ 04,32 SAY 'DUNS: '+ TRIM(XDUNS)
*    @ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
    *B500670,1 MFM 04/26/95 (End).
*  ENDIF
*  IF PRINTFCTR
*    @ 04,50 SAY 'R E M I T   T O:'
*  ENDIF

  * LINE 5
*  IF XINVNAME='Y'
*    @ 05,05 SAY HLINE3
*  ENDIF
*  IF PRINTFCTR
*    @ 05,50 SAY XFNAME
*  ENDIF

  * LINE 6
*  IF XINVNAME='Y'
*    @ 06,05 SAY HLINE4
*  ENDIF
*  IF PRINTFCTR
*    @ 06,50 SAY XFADDR1
*  ENDIF

  * LINE 7
*  IF XINVNAME='Y'
*    *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
    *HDM     @ 07,05 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
*    @ 07,05 SAY XPHONE SIZE 1,16
    *@ 07,05 SAY HLINE5
    *B500772,1 HISH 06/29/95.  ( End )
*  ENDIF
*  IF PRINTFCTR
*    @ 07,50 SAY XFADDR2
*  ENDIF

  * LINE 8
*  IF PRINTFCTR
*    @ 08,50 SAY XFADDR3
*  ENDIF

  *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
*  =lfSolSpAdr()
*  XBTNAME = lcSolTName
*  XBTADDR1 = laSoldTo[1]
*  XBTADDR2 = laSoldTo[2]
*  XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
*  IF LEN(TRIM(laSoldTo[2])) =0
*    XBTADDR2 = laSoldTo[3]
*    XBTADDR3 = ''
*  ENDIF

*  XSTNAME = lcShpTName
*  XSTADDR1 = laShipTo[1]
*  XSTADDR2 = laShipTo[2]
*  XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
*  IF LEN(TRIM(laShipTo[2])) =0
*    XSTADDR2 = laShipTo[3]
*    XSTADDR3 = ''
*  ENDIF
  *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]


*  @ 10,10 SAY XBTNAME
*  IF CURLOOP = '1'
*    @ 10,47 SAY XSTNAME
*  ENDIF

*  @ 11,10 SAY XBTADDR1
*  IF CURLOOP = '1'
*    @ 11,47 SAY XSTADDR1
*  ENDIF

*  @ 12,10 SAY XBTADDR2
*  IF CURLOOP = '1'
*    @ 12,47 SAY XSTADDR2
*  ENDIF

  *B602962,1 BWA  Modify the field dimension due to the attached form.
  *@ 13,10 SAY XBTADDR3
*  @ 13,10 SAY SUBSTR(XBTADDR3,1,35)
*  IF CURLOOP = '1'
    *@ 13,47 SAY XSTADDR3
*    @ 13,47 SAY SUBSTR(XSTADDR3,1,35)
*  ENDIF
  *B602962,1 BWA [END]

*  @ 17,01 SAY ACCOUNT
  *B602962,1 BWA  Modify the field dimension due to the attached form.
  *@ 17,09 SAY CUSTPO
*  @ 17,09 SAY SUBSTR(CUSTPO,1,10)
  *B801604,1 (Bebin) Say the DC if there is one.
  *@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
*  @ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
  *B801604,1 (End)
*  @ 17,30 SAY DEPT
*  @ 17,36 SAY ORDER
  *@ 17,44 SAY PTERMS
*  @ 17,44 SAY SUBSTR(PTERMS,1,13)
  *B602962,1 BWA  [END]
*  @ 17,60 SAY REP1
*  @ 17,64 SAY REP2
  *B602962,1 BWA  Modify the field dimension due to the attached form.
  *@ 17,68 SAY PSHIPVIA
*  @ 17,68 SAY SUBSTR(PSHIPVIA,1,15)
  *B602962,1 BWA  [END]

  && Put the header lines in a function to get them in any other place in the 
  && program to use the lines another time with the memo lines .
  =lfGetInHdr()
  *B602962,1 BWA [END]
  *------------------
  * PRINT SCALES
  *------------------

  *B602962,1 BWA Print the right scales and all the memo lines.
  SELE SCALE
  *SEEK 'S'        && BWA comment it for the scales 
  ROW = 19
  *X   = 1         && BWA comment it for the scales 
  *Y   = ' '
  
*  DO WHILE X <= 5 .AND. .NOT. EOF() .AND. TYPE='S'  
    *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
    *@ ROW,29 SAY SCALE+Y+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
    PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
    PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
    PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
  *B500670,1 MFM 04/26/95 (End).
  *B602962,1 BWA  Modify the field dimension due to the attached form.
  
  *B602962,1 BWA Print the right scales and all the memo lines.
  *@ ROW,22 SAY SUBSTR(SCALE,1,2)+PADR(SUBSTR(SZ1,1,3),4,' ')+PADR(SUBSTR(SZ2,1,3),4,' ')+;
    PADR(SUBSTR(SZ3,1,3),4,' ')+PADR(SUBSTR(SZ4,1,3),4,' ')+;
    PADR(SUBSTR(SZ5,1,3),4,' ')+PADR(SUBSTR(SZ6,1,3),4,' ')+;
    PADR(SUBSTR(SZ7,1,3),4,' ')+PADR(SUBSTR(SZ8,1,3),4,' ')

  *B602962,1 BWA  [END]
  *ROW = ROW + 1
  *X  = X + 1
  *SKIP
*ENDDO
  DIMENSION laScale[5]
  STORE SPACE (0) TO laScale
  lcAlias = ALIAS()
  lnCount = 1
  SELECT InvLine
  lcKey = invoice+STR(lineno,6)
  SEEK XINVOICE
  lnSclCnt = 0
  SCAN WHILE XINVOICE = INVOICE .AND. lnSclCnt =< 5
    IF ASCAN(laScale,INVLINE.SCALE) = 0 .AND. SEEK('S'+INVLINE.SCALE,'SCALE')
        lnSclCnt = lnSclCnt + 1
        DIMENSION laScale[lnSclCnt]
        lcScaleSz = SUBSTR(SCALE,1,3)+PADL(ALLTRIM(SCALE.SZ1),3,' ')+' '+PADL(ALLTRIM(SCALE.SZ2),3,' ')+;
                            ' '+PADL(ALLTRIM(SCALE.SZ3),3,' ')+' '+PADL(ALLTRIM(SCALE.SZ4),3,' ')+;
                            ' '+PADL(ALLTRIM(SCALE.SZ5),3,' ')+' '+PADL(ALLTRIM(SCALE.SZ6),3,' ')+;
                            ' '+PADL(ALLTRIM(SCALE.SZ7),3,' ')+' '+PADL(ALLTRIM(SCALE.SZ8),3,' ')   

        laScale[lnSclCnt] = lcScaleSz
    ENDIF
  ENDSCAN    
  SEEK(lcKey)     
  
  For lnCount =  1 TO ALEN(laScale,1)
    @ ROW,22 SAY laScale[lnCount]
    ROW = ROW + 1
  ENDFOR
  
SELECT (lcAlias)
*B602962,1 BWA [END]
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
    IF ROW >= MAXROW
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
  KEY = INVLINE->STYLE     &&+ INVLINE->COLOR
  SELECT STYLE
  SEEK KEY

  SELECT INVLINE
  *B602962,1 BWA  Modify the field dimension due to the attached form.
  *@ ROW,00 SAY SUBSTR(STYLE,1,7)
  *@ ROW,08 SAY SUBSTR(COLOR,1,3)                  &&TMI 06/16/94
  *@ ROW,12 SAY SUBSTR(STYLE->DESC,1,12)
  *@ ROW,25 SAY STYLE->SCALE
  *@ ROW,27 SAY QTY1   PICTURE '@Z 999'
  *@ ROW,31 SAY QTY2   PICTURE '@Z 999'
  *@ ROW,35 SAY QTY3   PICTURE '@Z 999'
  *@ ROW,39 SAY QTY4   PICTURE '@Z 999'
  *@ ROW,43 SAY QTY5   PICTURE '@Z 999'
  *@ ROW,47 SAY QTY6   PICTURE '@Z 999'
  *@ ROW,51 SAY QTY7   PICTURE '@Z 999'
  *@ ROW,55 SAY QTY8   PICTURE '@Z 999'
  *@ ROW,59 SAY TOTQTY PICTURE '9999'

  @ ROW,01 SAY SUBSTR(STYLE,1,12)
  @ ROW,15 SAY SUBSTR(STYLE,14,18)

  @ ROW,22 SAY SUBSTR(STYLE->SCALE,1,2)
  @ ROW,25 SAY PADL(QTY1,3,' ')   PICTURE '@Z 999'
  @ ROW,29 SAY PADL(QTY2,3,' ')   PICTURE '@Z 999'
  @ ROW,33 SAY PADL(QTY3,3,' ')   PICTURE '@Z 999'
  @ ROW,37 SAY PADL(QTY4,3,' ')   PICTURE '@Z 999'
  @ ROW,41 SAY PADL(QTY5,3,' ')   PICTURE '@Z 999'
  @ ROW,45 SAY PADL(QTY6,3,' ')   PICTURE '@Z 999'
  @ ROW,49 SAY PADL(QTY7,3,' ')   PICTURE '@Z 999'
  @ ROW,53 SAY PADL(QTY8,3,' ')   PICTURE '@Z 999'
  @ ROW,57 SAY TOTQTY PICTURE '9999'

  * CUM INVOICE TOTALS
  XLINETOT   = PRICE * TOTQTY
  XPIECES    = XPIECES + TOTQTY
  XSUBTOTAL  = XSUBTOTAL+XLINETOT

  *@ ROW,64  SAY PRICE     PICTURE '9999.99'
  @ ROW,62  SAY PRICE     PICTURE '99999.99'
  *B602962,1 BWA  [END]
  @ ROW,73  SAY XLINETOT  PICTURE '999999.99'
  ROW = ROW+1
  IF LINEUP
    EXIT
  ENDIF

  * GET NEXT LINE ITEM
  SELECT INVLINE
  SKIP
ENDDO
*** END LINE PROCESSING

SET DEVICE TO PRINT

*---------------------------------------------------------------
* CURRENTLY PROCESSING REGULAR LINE ITEMS
*---------------------------------------------------------------
ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

IF ENDPAGE = '1' .AND. !XNOTE_LOOP 
  @ ROW+1,12 SAY 'C O N T I N U E D ...'
  SELECT INVHDR
  *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
  @ 54,01 SAY XPHONE  SIZE 1,16
  @ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
  *@ 54,01 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
  *B500772,1 HISH 06/29/95.  ( End )

  IF LEN(TRIM(MSG1 + MSG2)) >0
    @ 56,10 SAY MSG1
    @ 57,10 SAY MSG2
  ENDIF
  @ 60,02 SAY INVHDR->PIKTKT
  
  *:B803127,1 MHM 06/26/2000 Print ship date instead of enter date[BEGIN]
  *@ 60,09 SAY INVDATE
  @ 60,09 SAY SHIPDATE
  *:B803127,1 MHM 06/26/2000 [END]
  
  @ 60,19 SAY '***'
  @ 60,25 SAY '*****'
  @ 60,35 SAY XINVOICE
  @ 60,73 SAY '******.**'
  
  LOOP
ENDIF 

*** Print invoice Notepad.

IF llNote
  SELECT NOTEPAD
  *E100207,1 YMA 03/30/95 (Begin).
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  *--HDM [Start] We have to seek in the notepad file first
  *--HDM         to keep track of the correct invoice notepad
  *IF TYPE + KEY <> 'C' + XINVOICE
  IF SEEK('C' + XINVOICE,'NOTEPAD')
    *--HDM [End]
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  ELSE
    lnMemLins = 0
  ENDIF

  IF TYPE + KEY = 'C' + XINVOICE
    @ ROW,02 SAY '* -- N O T E S -- *'
    ROW = ROW + 1

    *SCAN WHILE TYPE + KEY = 'C' + XINVOICE

    DO WHILE lnNotLine <= lnMemLins
      IF ROW >= MAXROW
        *B602962,1 BWA Print the right scales and all the memo lines.
        XNOTE_LOOP  = .T.
        *EXIT
        *B602962,1 BWA [END]
      ELSE
        XNOTE_LOOP = .F.
        *B602962,1 BWA Print the right scales and all the memo lines.
        *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
        *B602962,1 BWA [END]
        ROW = ROW + 1
      ENDIF
      *B602962,1 BWA Print the right scales and all the memo lines.
      IF ROW >= MAXROW      && if there is a line will de delete from 
        =lfGetcont()        && the memo field
        =lfGetInHdr()
      ENDIF
      *B602962,1 BWA [END]
      lnNotLine = lnNotLine + 1
    ENDDO

    IF !XNOTE_LOOP 
      @ ROW,02 SAY '* -- END OF NOTES -- *'
      lnNotLine = 1
      ROW = ROW + 1
    ELSE
      *B602962,1 BWA Collect all the footer lines in a function to call this function from any where from the program.
      *@ ROW+1,12 SAY 'C O N T I N U E D ...'
	  *SELECT INVHDR

      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
      *@ 54,01 SAY XPHONE SIZE 1,16
      *@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *@ 54,01 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *B500772,1 HISH 06/29/95.  ( End )

      *IF LEN(TRIM(MSG1 + MSG2)) >0
      *  @ 56,10 SAY MSG1
      *  @ 57,10 SAY MSG2
      *ENDIF
      *@ 60,02 SAY INVHDR->PIKTKT
      *@ 60,09 SAY INVHDR->INVDATE
      *@ 60,19 SAY '***'
      *@ 60,25 SAY '*****'
      *@ 60,35 SAY XINVOICE
      *@ 60,73 SAY '******.**'
      
      && Put the footer lines in a function to get them in any other place in the 
      && program to use the lines another time with the memo lines .
      =lfGetcont()
      *B602962,1 BWA [END]
      LOOP
    ENDIF
  ENDIF

  SET MEMOWIDTH TO lnOldMemW
  *E100207,1 YMA 03/30/95 (End).
ENDIF
SELECT INVHDR
@ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
*B602962,1 BWA  Modify the field dimension due to the attached form.
*@ ROW,57 SAY XPIECES   PICTURE '999999'
@ ROW,55 SAY XPIECES   PICTURE '999999'
*B602962,1 BWA  [END]
@ ROW,72 SAY XSUBTOTAL PICTURE '9999999.99'
ROW=ROW+1
IF DISCOUNT<>0
  @ ROW,12 SAY 'TOTAL - D I S C O U N T'
  @ ROW,71 SAY DISCOUNT PICTURE '99999999.99'
  ROW=ROW+1
ENDIF
IF COD <> 0
  @ ROW,12 SAY 'TOTAL - C O D '
  @ ROW,72 SAY COD PICTURE '9999999.99'
  ROW=ROW+1
ENDIF
IF INSUR <> 0
  @ ROW,12 SAY 'TOTAL - I N S U R A N C E'
  @ ROW,72 SAY INSUR PICTURE '9999999.99'
  ROW=ROW+1
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
  @ ROW,72 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
  ROW = ROW + 1
  *B800080,1 TAK 08/16/95 Start, Added PST tax.
  IF InvHdr.nPSTAmt <> 0
    @ Row,12 SAY 'P S T   T A X'
    @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
    @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
    Row = Row + 1
  ENDIF
  *B800080,1 TAK 08/16/95 End.
  *C102212,1 (Begin) Add HST Tax.
  IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
    @ Row,12 SAY 'H S T   T A X'
    @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
    @ Row,72 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
    Row = Row + 1
  ENDIF  
  *C102212,1 (End)

ENDIF

IF FREIGHT <> 0
  @ ROW,12 SAY 'TOTAL - F R E I G H T'
  @ ROW,72 SAY FREIGHT PICTURE '9999999.99'
  ROW=ROW+1
ENDIF

*** Print the tax rate and tax amount
IF XTAX .AND. XTAX_METH = 'A'
  XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
  @ ROW,12 SAY XTAX_DESC
  *-- TMI 01/17/95
  IF !EMPTY(lcTaxRefr)
    @ Row,33 SAY lcTaxRefr
  ENDIF
  *-- End TMI 01/17/95
  @ ROW,64 SAY XSTRING_RATE + '%'
  @ ROW,72 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
  ROW = ROW + 1

  *B800080,1 TAK 08/16/95 Start, Added PST tax.
  IF InvHdr.nPSTAmt <> 0
    @ Row,12 SAY 'P S T   T A X'
    @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
    @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
    Row = Row + 1
  ENDIF
  *B800080,1 TAK 08/16/95 End.
  *C102212,1 (Begin) Add HST Tax.
  IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
    @ Row,12 SAY 'H S T   T A X'
    @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
    @ Row,72 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
    Row = Row + 1
  ENDIF  
  *C102212,1 (End)
ENDIF

*B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
@ 54,01 SAY XPHONE SIZE 1,16
@ 54,18 SAY XNOTE1 + ' ' + XNOTE2
*@ 54,01 SAY XPHONE + ' ' + XNOTE1 + ' ' + XNOTE2
*B500772,1 HISH 06/29/95.  ( End )

IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF

IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
  @ 59,56 SAY 'APPROVAL: ' + INVHDR->APPROVAL
ENDIF

@ 60,02 SAY INVHDR->PIKTKT

*:B803127,1 MHM 06/26/2000 Print ship date instead of enter date[BEGIN]
*@ 60,09 SAY INVDATE
@ 60,09 SAY SHIPDATE
*:B803127,1 MHM 06/26/2000 [End]

@ 60,19 SAY CARTONS  PICTURE '@Z 999'

*B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
*@ 60,25 SAY WEIGHT   PICTURE '@Z 99999'
@ 60,25 SAY WEIGHT   PICTURE '@Z 99.99'
*B607103,1 KHM 05/04/2003 (End)

@ 60,35 SAY INVHDR->INVOICE
@ 60,72 SAY INVHDR->TOTALCHG PICTURE '9999999.99'
SELECT INVHDR
*B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
*REPLACE PRTFLAG WITH 'P'
*B603487,1 BWA 03/08/2000 [END]
SELECT INVHDR

IF EOF()
  NEWDOC = .F.
  SET DEVICE TO SCREEN
  RETURN
ELSE
  NEWDOC = .T.
ENDIF

  *B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *B803585,1 [END]

ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810D.PRG
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
  
*!*************************************************************
*! Name : lpGtScl.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! Date : 05/11/94.
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
*B600987,1 MFM 02/28/96 (Begin) Build the current record expr. string,
*B600987,1 MFM          instead of saving the record number.
lcExp   = Invoice + STR(LineNo,6)
*lnRecNo = RECNO()
*B600987,1 MFM 02/28/96 (End).

lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + Scale
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN

*B600987,1 MFM 02/28/96 (Begin) Seek for the saved expr. instead of going to 
*B600987,1 MFM          the record number.
= SEEK(lcExp,"InvLine")
*GOTO lnRecNo
*B600987,1 MFM 02/28/96 (End).

*-- Select the old alias.
SELECT (lcAlias)

*-- EOF( lpGtScl )

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
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
*:Modifications:
*:B602962,1 BWA Collect all the header lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetInHdr

SELECT INVHDR
@ 02,31 SAY INVDATE
@ 02,41 SAY 'INVOICE: '+XINVOICE
@ 02,59 SAY 'PO: '+INVHDR->CUSTPO
IF XINVNAME='Y'
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF

* LINE 4
IF XINVNAME='Y'
  @ 04,05 SAY HLINE2
ENDIF
IF !EMPTY(XDUNS)
  @ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
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
  @ 07,05 SAY XPHONE SIZE 1,16
ENDIF
IF PRINTFCTR
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF PRINTFCTR
  @ 08,50 SAY XFADDR3
ENDIF

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
@ 13,10 SAY SUBSTR(XBTADDR3,1,35)
IF CURLOOP = '1'
  @ 13,47 SAY SUBSTR(XSTADDR3,1,35)
ENDIF
@ 17,01 SAY ACCOUNT
@ 17,09 SAY SUBSTR(CUSTPO,1,10)
@ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY SUBSTR(PTERMS,1,13)
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,68 SAY SUBSTR(PSHIPVIA,1,15)
ROW = 24
  
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
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
*! Example     : = lfGetInHdr()
*:Modifications:
*:B602962,1 BWA Collect all the footer lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetcont
@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR

@ 54,01 SAY XPHONE SIZE 1,16
@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,02 SAY INVHDR->PIKTKT

*:B803127,1 MHM 06/26/2000 Print ship date instead of enter date[BEGIN]
*@ 60,09 SAY INVHDR->INVDATE
@ 60,09 SAY INVHDR->SHIPDATE
*:B803127,1 MHM 06/26/2000 [End]

@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,73 SAY '******.**'
