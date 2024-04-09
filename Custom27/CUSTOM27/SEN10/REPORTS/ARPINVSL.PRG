*:************************************************************************
*: Program file  : ARPINVSL.PRG 
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: RAMY MABROUK
*:  Procs & Fncts: gfGetZone(),gfGetMemVar(),gfOpenFile(),gfCodDes()
*:  Documented    12/26/1999
*:           Note: This program is a modification to standard form 'H' 
*:                 due to C101723
*:************************************************************************
*: Modifications :
*: B803846,1 BWA 11/16/2000 Modify the alignment for the invoice.
*: B604040,1 BWA 11/22/2000 Some modifications in the invoice due to mail sent.
*: B803871,1 ASH 11/29/2000 Some modifications in the invoice.
*:************************************************************************

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

lcAlias = Alias()
=gfOpenFile(gcDataDir+'OrdLine',gcDataDir+'Ordlinst','SH')
=gfOpenFile(gcDataDir+'ConsInvL' , gcDataDir+'ConsInvL' , 'SH')

SELECT  ConsInvL
SET RELATION TO 'O' + ORDER + STORE + STYLE INTO ORDLINE ADDITIVE
SELECT (lcAlias)


llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)

NEWDOC = .T.

MAXROW = 47

lcDist = ' '


IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

llNote = llRpInvNot
lnNotLine = 1

SELECT INVHDR
LINEUP = .F.
STORE TRIM(QCOMPANY)                                       TO HLINE1
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])                                   TO HLINE4
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

XINVNAME = lcPrnComp

gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SCAN FOR &lcASExp   WHILE INKEY() <> 32
  WAIT WINDOW 'Selecting Invoices for printing ...' + Invoice NOWAIT
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
    IF EOF()
      SELECT (INVHTEMP)
      SKIP
      LOOP
    ENDIF
    NEWDOC = .F.
    XORDER = ORDER
    XPHONE    = laCompAdd[5]
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    lcScale1 = ""
    lcScale2 = ""
    
    *** GET THE BILL TO AND SHIP ADDRESS
    SELE CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    
    XBTNAME  = BTNAME
    XBTADDR1 = CADDRESS1
    XBTADDR2 = CADDRESS2
    XBTADDR3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF
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
      IF INVHDR.CONSOL = 'Y'       
        SELECT CONSINVH
        SEEK XINVOICE
        SELECT CUSTOMER
        SEEK 'S'+XACCOUNT+CONSINVH->STORE
      ENDIF

      SELE CUSTOMER
      IF !EMPTY(DIST_CTR)
        lcDist = DIST_CTR
        = SEEK('S'+XACCOUNT+lcDist)
      ENDIF

      XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
      XSTADDR1 = CADDRESS12
      XSTADDR2 = CADDRESS22
      XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ENDIF

    ****** FIND THE INVOICE LINES
    SELECT INVLINE
    SEEK XINVOICE
    IF EOF()
      PRTERR = 2
    ENDIF
    *** GET THE DESCRIPTION ABOUT THE CODES
      *** GET THE DESCRIPTION ABOUT THE CODES
      
      SELECT CODES
      SET ORDER TO CODES IN CODES 
      PTERMS = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')    &&TERMS 
      PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')      &&SHIPVIA
      PSPCINST= gfCodDes(INVHDR.SPCINST,'SPCINST')       &&SPC INST.
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
      
*      XZN = laZone[1,2]

    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------

    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  
  SELECT INVHDR
  *-----------------
  * START PRINT
  *-----------------

  *B604040,1 BWA 11/22/2000 [START]
  *@ 03,63 SAY XINVOICE
  *B803871,1 ASH 11/29/2000 [START] Move the invoice # 2 chrs right.
  *@ 03,58 SAY XINVOICE
  @ 03,60 SAY XINVOICE
  *B803871,1 ASH 11/29/2000 [End]
  *B604040,1 BWA 11/22/2000 [END]

  @ 03,75 SAY INVDATE
  @ 05,75 SAY SUBSTR( CDIVISION, 1, 14 )  

  IF XINVNAME='Y' AND !EMPTY(lcDunsNo)
    @ 06,05 SAY 'Duns# :'
    @ 06,13 SAY lcDunsNo
  ENDIF

  =lfSolSpAdr()
  XBTNAME  = lcSolTName
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

  SELECT INVHDR
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 07,14 SAY "Remit to :"
    *@ 07,25 SAY lcFacName 

    @ 07,06 SAY "Remit to :"
    @ 07,16 SAY lcFacName 
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF  
  
  @ 07,75 SAY ORDER
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 08,25 SAY laFactor[1]
    @ 08,16 SAY laFactor[1]
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF

  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 09,25 SAY laFactor[2]
    @ 09,16 SAY laFactor[2]
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF

  @ 09,75 SAY PIKTKT
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 10,25 SAY laFactor[3]
    @ 10,16 SAY laFactor[3]
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
    @ 11,16 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF
  
  @ 11,75 SAY APPROVAL  
 
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    *B604040,1 BWA 11/22/2000 [START]
    *@ 12,25 SAY "Ref. Account:"
    *@ 12,39 SAY CUSTOMER.FACTACCT

    @ 12,16 SAY "Ref. Account:"
    @ 12,31 SAY CUSTOMER.FACTACCT
    *B604040,1 BWA 11/22/2000 [END]
  ENDIF

  *B604040,1 BWA 11/22/2000 [START]
  *@ 15,07 SAY XBTNAME
  *@ 15,58 SAY XSTNAME
  *@ 16,07 SAY XBTADDR1
  *@ 16,58 SAY SUBSTR(XSTADDR1,1,25)
  *@ 17,07 SAY XBTADDR2
  *@ 17,58 SAY SUBSTR(XSTADDR2,1,25)
  *@ 18,07 SAY SUBSTR(XBTADDR3,1,45)
  *@ 18,58 SAY SUBSTR(XSTADDR3,1,29)
  *@ 23,05 SAY ACCOUNT

  *B803846,1 BWA 11/16/2000  Modify the alignement for the invoice.[START]
  *@ 23,18 SAY LEFT(CUSTPO,10)

  *B604040,1 BWA 11/22/2000 [START]
  *@ 23,14 SAY LEFT(CUSTPO,10)
  *B604040,BWA 11/22/2000 [END]

  *B803846,1 [END]

  *@ 23,18 SAY LEFT(CUSTPO,10)
  *@ 23,26 SAY STORE
  *@ 23,37 SAY DEPT
  *@ 23,44 SAY LEFT(PTERMS,21)
  *@ 23,66 SAY REP1                         
  *@ 23,70 SAY REP2
  *@ 23,75 SAY SUBSTR(PSHIPVIA,1,13)

  @ 15,06 SAY XBTNAME
  @ 15,57 SAY XSTNAME
  @ 16,06 SAY XBTADDR1
  @ 16,57 SAY SUBSTR(XSTADDR1,1,25)
  @ 17,06 SAY XBTADDR2
  @ 17,57 SAY SUBSTR(XSTADDR2,1,25)
  @ 18,06 SAY SUBSTR(XBTADDR3,1,45)
  @ 18,57 SAY SUBSTR(XSTADDR3,1,29)
  @ 23,02 SAY ACCOUNT
  @ 23,14 SAY LEFT(CUSTPO,10)
  @ 23,24 SAY STORE
  @ 23,34 SAY DEPT
  @ 23,41 SAY LEFT(PTERMS,21)
  @ 23,64 SAY REP1                         
  @ 23,67 SAY REP2
  @ 23,72 SAY SUBSTR(PSHIPVIA,1,13)
  *B604040,1 BWA 11/22/2000 [END]

  * PRINT SCALES
  
  ROW = 26
  
  SELECT INVLINE
  IF !XNOTE_LOOP
    trec1=RECNO()
    tscale1=" "
    tscale2=" "
    DO WHILE .T.
      IF Invoice<>xinvoice .or. (tscale1<>" " .and. tscale2<>" ")
        EXIT
      ENDIF
      IF tscale1=" "
        tscale1=scale
      ENDIF
      IF scale<>tscale1
        tscale2=scale
      ENDIF
      SKIP
    ENDDO

    SELECT INVLINE
    SEEK XINVOICE
  ENDIF                          

  SELECT SCALE
  trec1=RECNO()
  i=49
  Y=' '
  do while i < 51
    ti=chr(i)
    SEEK 'S'+TSCALE&TI                    
    if !eof()
      *B803846,1 BWA 11/16/2000  Modify the alignment for the invoice.[START]
      *@ ROW,20 SAY SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                                 PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                                 PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')

       @ ROW,20 SAY LEFT(SCALE,1)+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                                  PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                                  PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')

      *B803846,1 [END]
      row=row+1
    endif
    
    IF EMPTY(lcScale1)
      lcScale1  =  SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                         PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                         PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')

    ELSE

      lcScale2  =  SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                         PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                         PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')

    ENDIF
    
    i=i+1
  ENDDO

  SELECT INVLINE
  SEEK XINVOICE  
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 30
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP           
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
         IF ROW >= MAXROW      && if there is a line will de delete from 
           =lfGetcont()        && the memo field
           =lfGetInHdr()
           LOOP
         ENDIF
        EXIT         
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE->STYLE           &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY
      XSTYDESC = IIF(FOUND() , SUBSTR(DESC,1,17) ,'')

      SELECT INVLINE

      *IF INVHDR.CONSOL = 'Y'
      *  lcAliac = ALIAS()
      *  SELECT CONSINVL
      *  =SEEK('O' + ORDER + STORE + STYLE , 'ORDLINE')
      *  SELECT (lcAliac)
      *ELSE
      *  =SEEK('O' + ORDER + STORE + STYLE , 'ORDLINE')
      *ENDIF
      IF INVHDR.CONSOL = 'Y'
        IF SEEK(INVOICE , 'CONSINVL')
          lcAliac = ALIAS()
          SELECT CONSINVL
          lnBO = 0
          lnTO = 0
          SCAN FOR INVOICE = INVLINE.INVOICE AND STYLE = INVLINE.STYLE
            lnBO = lnBO + ORDLINE.TotQty
            lnTO = lnTO + ORDLINE.TotBook
          ENDSCAN
        ENDIF
        SELECT (lcAliac)
      ELSE
        =SEEK('O' + ORDER + STORE + STYLE , 'ORDLINE')
        lnBO = ORDLINE.TotQty
        lnTO = ORDLINE.TotBook
      ENDIF
      
      @ ROW,04 SAY SUBSTR(STYLE,1,10)
      @ ROW,14 SAY SUBSTR(STYLE,14,18)

      *B604040,1 BWA 11/22/2000 [START]
      *@ ROW,20 SAY STYLE.SCALE
      *@ ROW,23 SAY IIF(QTY1=0,'',PADC(QTY1,5,' '))   PICTURE '99999'
      *@ ROW,28 SAY IIF(QTY2=0,'',PADC(QTY2,5,' '))   PICTURE '99999'
      *@ ROW,33 SAY IIF(QTY3=0,'',PADC(QTY3,5,' '))   PICTURE '99999'
      *@ ROW,38 SAY IIF(QTY4=0,'',PADC(QTY4,5,' '))   PICTURE '99999'
      *@ ROW,43 SAY IIF(QTY5=0,'',PADC(QTY5,5,' '))   PICTURE '99999'
      *@ ROW,48 SAY IIF(QTY6=0,'',PADC(QTY6,5,' '))   PICTURE '99999'
      *@ ROW,51 SAY lnTO                              PICTURE '99999'
      *@ ROW,56 SAY lnBO                 			 PICTURE '99999'
      *@ ROW,61 SAY TotQty                            PICTURE '99999'

      @ ROW,20 SAY LEFT(STYLE.SCALE,1)
      @ ROW,22 SAY IIF(QTY1=0,'',PADC(QTY1,5,' '))   PICTURE '99999'
      @ ROW,27 SAY IIF(QTY2=0,'',PADC(QTY2,5,' '))   PICTURE '99999'
      @ ROW,32 SAY IIF(QTY3=0,'',PADC(QTY3,5,' '))   PICTURE '99999'
      @ ROW,37 SAY IIF(QTY4=0,'',PADC(QTY4,5,' '))   PICTURE '99999'
      @ ROW,42 SAY IIF(QTY5=0,'',PADC(QTY5,5,' '))   PICTURE '99999'
      @ ROW,47 SAY IIF(QTY6=0,'',PADC(QTY6,5,' '))   PICTURE '99999'
      @ ROW,50 SAY lnTO                              PICTURE '99999'
      @ ROW,55 SAY lnBO                 			 PICTURE '99999'
      @ ROW,60 SAY TotQty                            PICTURE '99999'
      *B604040 [END]

      * SUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      *B803871,1 ASH 11/29/2000 [Start] Shift the price&amount to the left.
      *@ ROW,68  SAY PRICE     PICTURE '99999.99'
      *@ ROW,76  SAY XLINETOT  PICTURE '999999.99'
      @ ROW,65  SAY PRICE     PICTURE '99999.99'
      @ ROW,72  SAY XLINETOT  PICTURE '999999.99'
      *B803871,1 ASH 11/29/2000 [End]
      

      ROW = ROW+1
      @ ROW,04  SAY  INVLINE.dESC1
      ROW = ROW+1
      DO lpPrtSku      &&Procedure to print the Style/Color Sku no.

      SELECT INVLINE
      SKIP
   ENDDO

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP                 
     =lfGetcont()
     LOOP
   ENDIF
   
   IF llNote
     SELECT NOTEPAD
     lnOldMemW = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75
     IF TYPE + KEY <> 'C' + XINVOICE
       SEEK 'C' + XINVOICE
       lnMemLins = MEMLINES(NOTEPAD.MNOTES)
     ENDIF
     IF TYPE + KEY = 'C' + XINVOICE
       @ ROW,02 SAY '* -- N O T E S -- *' 
       ROW = ROW + 1 

       DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
         IF ROW >= MAXROW
           XNOTE_LOOP = .T.
         ELSE
           XNOTE_LOOP = .F.
           lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
           IF  LEFT(lcCurrLine,1) # '*'
             @ ROW,02 SAY lcCurrLine
             ROW = ROW + 1
           ENDIF
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
         =lfGetcont()
         LOOP
       ENDIF
     ENDIF
     SET MEMOWIDTH TO lnOldMemW
  ENDIF
   @ 52,09 SAY INVHDR.PHONE  PICTURE lcPhonPict SIZE 1,16

   SELECT INVHDR
   @ 52,56 SAY 'MERCHANDISE'
   @ 52,74 SAY XSUBTOTAL            PICTURE '9999999.99'  
   ROW=53
   @ 53,07 SAY INVHDR.NOTE1

   IF XTAX .AND. XTAX_METH = 'M' .AND. INVHDR.TAX_AMT # 0
     IF !EMPTY(lcTaxRefr)
       @ Row,55 SAY lcTaxRefr
       Row = Row + 1
     ENDIF  
     XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
     @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
     @ ROW ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
     ROW = ROW + 1
   ENDIF  
   
   IF ROW = 55
     @ 55,07 SAY INVHDR.NOTE2
     llNotePrnt = .T.
   ELSE
     llNotePrnt = .F.  
   ENDIF

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,55 SAY 'TOTAL - FREIGHT'
      @ ROW,74 SAY WKAMT       PICTURE '9999999.99'  
      ROW=ROW+1
   ENDIF

   IF DISCOUNT<>0
      @ ROW,55 SAY 'DISCOUNT'
      @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
   ENDIF

   IF !llNotePrnt
     @ 55,07 SAY INVHDR.NOTE2
   ENDIF

   IF XTAX .AND. XTAX_METH = 'A' .AND. INVHDR.TAX_AMT # 0
     @ 56,55 SAY lcTaxRefr
     XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
     @ 57 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
     @ 57 ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
   ENDIF

   IF xTAx .AND. llIsCanada 
     lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))
     lnRow = IIF (xTAx_Meth='A',55,54)
     @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
     @ lnRow ,74 SAY InvHdr.nPstAmt PICTURE '9999999.99'
   ENDIF

   *B604040,1 BWA 11/22/2000 [START]
   *@ 56,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
   *@ 56,65 SAY XPIECES           PICTURE '9999999'
   *@ 56,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'

   @ 58,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
   @ 58,65 SAY XPIECES           PICTURE '9999999'
   @ 58,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'
   *B604040,1 BWA 11/22/2000 [END]

   *-- GET NEXT HEADER RECORD
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
*! Name : gfGetZone.
*! Auth : Tarek Mohamed Ismael (TMI).
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


*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Timour Abdalla Khalil.
*! Date : 07/20/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
PROCEDURE lpPrtSku

IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
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

  DIME laSku[6]
  laSku = SPACE(16)
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
     FOR lnX=1 TO 6
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
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 7
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()

      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 3 .OR. lnI = 7
        EXIT
      ENDIF
    ENDDO
    
    @ ROW,04 SAY lcStrToPrn
    
    ROW = ROW + 1
  ENDDO  
ELSE
  @ ROW,04 SAY Sku
  @ ROW,20 SAY 'CUSTOMER SKU #'

  ROW=ROW+1
ENDIF
RETURN

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
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lnSelFile

@ 03,63 SAY XINVOICE
@ 03,75 SAY INVDATE
@ 05,75 SAY SUBSTR( CDIVISION, 1, 14 )

IF XINVNAME='Y' AND !EMPTY(lcDunsNo)  
  @ 06,05 SAY 'Duns# :'
  @ 06,13 SAY lcDunsNo
ENDIF


=lfSolSpAdr()
XBTNAME  = lcSolTName
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
SELECT INVHDR

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 07,14 SAY "Remit to :"
  @ 07,25 SAY lcFacName 
ENDIF


@ 07,75 SAY ORDER

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 08,25 SAY laFactor[1]
  @ 09,25 SAY laFactor[2]
ENDIF

@ 09,75 SAY PIKTKT  


IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 10,25 SAY laFactor[3]
  @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
ENDIF

@ 11,75 SAY APPROVAL


@ 14,25 SAY "Ref. Account:"
@ 14,39 SAY CUSTOMER.FACTACCT
@ 15,07 SAY XBTNAME
@ 15,58 SAY XSTNAME
@ 16,07 SAY XBTADDR1
@ 16,58 SAY SUBSTR(XSTADDR1,1,25)
@ 17,07 SAY XBTADDR2
@ 17,58 SAY SUBSTR(XSTADDR2,1,25)
@ 18,07 SAY SUBSTR(XBTADDR3,1,45)
@ 18,58 SAY SUBSTR(XSTADDR3,1,29)
@ 23,05 SAY ACCOUNT
@ 23,18 SAY LEFT(CUSTPO,10)
@ 23,26 SAY STORE
@ 23,37 SAY DEPT
@ 23,44 SAY LEFT(PTERMS,21)
@ 23,66 SAY REP1                         
@ 23,70 SAY REP2
@ 23,75 SAY SUBSTR(PSHIPVIA,1,13)

ROW = 23

@ 25,20 SAY lcScale1
@ 26,20 SAY lcScale2

ROW = 29
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
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR

*B803846,1 BWA 11/16/2000  Modify the alignment for the invoice.[START]
*@ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
*B604040,1 BWA 11/22/2000 [START]
*@ 51,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
*B604040,1 BWA 11/22/2000 [END]
*B803846,1 [END]

*B604040,1 BWA 11/22/2000 [START]
*@ 50,56 SAY 'MERCHANDISE'
*@ 50,75 SAY '**********'
*ROW = ROW + 1
*@ 51,07 SAY INVHDR.NOTE1
*@ 53,07 SAY INVHDR.NOTE2
*@ 56,16 SAY INVHDR.CARTONS PICTURE '@Z 999'
*@ 56,65 SAY '*******'
*@ 56,75 SAY '**********'

@ 52,09 SAY INVHDR.PHONE  PICTURE lcPhonPict SIZE 1,16
@ 52,56 SAY 'MERCHANDISE'
@ 52,75 SAY '**********'
ROW = ROW + 1
@ 53,07 SAY INVHDR.NOTE1
@ 55,07 SAY INVHDR.NOTE2
@ 58,16 SAY INVHDR.CARTONS PICTURE '@Z 999'
@ 58,65 SAY '*******'
@ 58,75 SAY '**********'
*B604040,1 BWA 11/22/2000 [END]