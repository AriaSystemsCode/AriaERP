****************************************************************************
*: Program file  : ARPINVPA.PRG (C# 101652)
*: Program desc. : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES
*:               : (Cust.: PMA100)
*:               : Convert INV810z from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:       : PROCEDURES :
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*
llNoRec = .F.
STORE 00 TO lnNonMajSt
STORE '' TO lcNonMajPi,lcNonMajTl,lcStyGrp,lcStyMajor
STORE '' TO lnMajorLen ,lnFreeLen , lnColorLen
*-- Get the style major and color [Begin.]
*-- Get the color
STORE 0 TO lnColorLen,lnNonMajSt

*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- lnMajor Var. is declared in main prg (ARPINV) 
*-- Get the Non Major elements. [Begin.]
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnNonMajSt = laMajSeg[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSeg[lnI,1]='C',;
                 laMajSeg[lnI,3],;
                 lnColorLen + laMajSeg[lnI-1,6] + laMajSeg[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*-- Get the Non Major elements. [End.]

NEWDOC = .T.
MAXROW = 47
DECLARE laSoldTo[5,1] , laShipTo[5,1]
INVHTEMP  = gfTempName()
SELECT INVHDR
COPY ALL FOR &lcRpExp TO &gcWorkDir.&INVHTEMP
= gfOpenFile('&gcWorkDir.&INVHTEMP',' ','EX')
SELECT (INVHTEMP)
GO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  IF USED(INVHTEMP)
    USE IN (INVHTEMP)
  ENDIF
  ERASE &gcWorkDir.&INVHTEMP+'.DBF'
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ELSE
  *-- Message : --LINE UP <YES>  <NO>  <QUIT>--
  llLineUp = gfModalGen('QRM40145B40012','DIALOG' )
  DO CASE
    CASE  llLineUp = 3
      IF USED(INVHTEMP)
        USE IN (INVHTEMP)
      ENDIF
      ERASE &gcWorkDir.&INVHTEMP+'.DBF'
      llNoRec = .T.
      SET DEVICE TO SCREEN
      RETURN
    CASE  llLineUp = 2
      llLineUp = .F.
    CASE  llLineUp = 1
      llLineUp = .T.
  ENDCASE
ENDIF
SELECT &INVHTEMP
WAIT WINDOW  'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
*** MAIN LOOP
DO WHILE INKEY() <>32
  SELECT &INVHTEMP
  IF EOF()
    EXIT
  ENDIF
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
    =SEEK(XINVOICE)
    IF EOF()
      SELECT &INVHTEMP
      SKIP
      LOOP
    ENDIF
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = IIF(EMPTY(PHONE),'',TRANSFORM(PHONE,lcPhonPict))
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *** GET THE BILL TO AND SHIP ADDRESS
    =lfSolSpAdr()
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = laSoldTo[3]

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = laShipTo[3]

    *** FIND THE INVOICE LINES
    SELECT INVLINE
    =SEEK(XINVOICE)
    IF EOF()
      PRTERR = 2
    ENDIF
    *** GET THE DESCRIPTION ABOUT THE CODES

    SELECT CODES

    *-- To get the payment Terms.
    PTERMS = SUBSTR(gfCodDes(INVHDR.CTERMCODE,'CTERMCODE',.T.),1,15)
    lcZone = ''
    DIMENSION laZone[1,2]
    laZone[1,1]     = 'CUPS'
    laZone[1,2]     = 'lcZone'
    PSHIPVIA = SUBSTR(gfCodDes(INVHDR.SHIPVIA,'SHIPVIA',.T.),1,15)
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    DO CASE
      CASE 'G' $ lcZone
        XZN  = '('+INVHDR.UPSZONE+')'
      CASE '2' $ lcZone
        XZN  = '(12)'
      CASE 'N' $ lcZone
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE
    PSHIPVIA  = TRIM(PSHIPVIA)+XZN
    PSPCINST  = SUBSTR(gfCodDes(INVHDR.SPCINST,'SPCINST'),1,15)
    PDIVISION = gfCodDes(XDIVISION,'CDIVISION')

    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    PRINTFCTR = llPrnFact
    STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
    DO WHILE PRINTFCTR
      IF INVHDR.CFACCODE = ' '
        PRINTFCTR = .F.
        EXIT
      ENDIF
      SELECT SYCFACT          && this File contains all the FACTORS for all customers
      SEEK INVHDR.CFACCODE
      DO CASE
        CASE EOF()
          PRINTFCTR = .F.
          STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
        CASE FOUND()
          XFNAME  = CFACCOMP
          XFADDR1 = CADDRESS1
          XFADDR2 = CADDRESS2
          XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
          IF XFADDR2 = ' '
            XFADDR2 = XFADDR3
            XFADDR3 = ' '
          ENDIF
      ENDCASE
      EXIT
    ENDDO
    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  *** END NEWDOC
  IF PRTERR >0
    SELE &INVHTEMP
    SKIP
    LOOP
  ENDIF
  SELECT INVHDR
  *-----------------
  * START PRINT
  *-----------------
  @ 01,60 SAY PIKTKT
  @ 01,73 SAY SUBSTR(PDIVISION,1,12)
  @ 03,73 SAY INVDATE
  @ 05,73 SAY ORDER
  @ 07,73 SAY XINVOICE
  @ 09,73 SAY APPROVAL
  @ 13,09 SAY XBTNAME
  @ 13,56 SAY XSTNAME
  @ 14,09 SAY XBTADDR1
  @ 14,56 SAY XSTADDR1
  @ 15,09 SAY XBTADDR2
  @ 15,56 SAY XSTADDR2
  @ 16,09 SAY XBTADDR3
  @ 16,56 SAY XSTADDR3
  @ 21,00 SAY ACCOUNT
  @ 21,12 SAY CUSTPO
  @ 21,25 SAY STORE
  @ 21,33 SAY DEPT
  @ 21,41 SAY PTERMS
  @ 21,62 SAY REP1
  @ 21,66 SAY REP2
  @ 21,71 SAY SUBSTR(PSHIPVIA,1,12)

  * PRINT SCALES
  ROW = 23
  SELECT INVLINE
  TREC1=RECNO()
  TSCALE1=" "
  TSCALE2=" "
  DO WHILE .T.
    IF INVOICE<>XINVOICE .OR. (TSCALE1<>" " .AND. TSCALE2<>" ")
      EXIT
    ENDIF
    IF TSCALE1=" "
      TSCALE1=SCALE
    ENDIF
    IF SCALE<>TSCALE1
      TSCALE2=SCALE
    ENDIF
    SKIP
  ENDDO
  GOTO TREC1
  SELECT SCALE
  GOTO TOP
  TREC1=RECNO()
  I=49
  DO WHILE I<51
    TI=CHR(I)
    =SEEK('S'+TSCALE&TI)
    IF !EOF()
      @ ROW,29 SAY SCALE
      @ ROW,32 SAY SUBSTR(ALLTRIM(SZ1),1,3)
      @ ROW,36 SAY SUBSTR(ALLTRIM(SZ2),1,3)
      @ ROW,40 SAY SUBSTR(ALLTRIM(SZ3),1,3)
      @ ROW,44 SAY SUBSTR(ALLTRIM(SZ4),1,3)
      @ ROW,48 SAY SUBSTR(ALLTRIM(SZ5),1,3)
      @ ROW,52 SAY SUBSTR(ALLTRIM(SZ6),1,3)
      @ ROW,56 SAY SUBSTR(ALLTRIM(SZ7),1,3)
      @ ROW,60 SAY SUBSTR(ALLTRIM(SZ8),1,3)
      ROW=ROW+1
    ENDIF
    I=I+1
  ENDDO
  GOTO TREC1
  CNT = 1

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 27 
   DO WHILE CURLOOP = '1'
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW>=47
         EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE.STYLE
      SELECT STYLE
      =SEEK(KEY)
      XSTYDESC = IIF(FOUND() , SUBSTR(DESC,1,16) ,'')
      SELECT INVLINE
      @ ROW,00 SAY SUBSTR(STYLE,1,7)
      @ ROW,08 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen-3)
      @ ROW,11 SAY XSTYDESC
      @ ROW,29 SAY SUBSTR(STYLE.SCALE,1,1)
      @ ROW,31 SAY QTY1   PICTURE '@Z 9999'
      @ ROW,35 SAY QTY2   PICTURE '@Z 9999'  
      @ ROW,39 SAY QTY3   PICTURE '@Z 9999'  
      @ ROW,43 SAY QTY4   PICTURE '@Z 9999'
      @ ROW,47 SAY QTY5   PICTURE '@Z 9999'   
      @ ROW,51 SAY QTY6   PICTURE '@Z 9999'  
      @ ROW,55 SAY QTY7   PICTURE '@Z 9999'   
      @ ROW,59 SAY QTY8   PICTURE '@Z 9999'
      @ ROW,63 SAY TOTQTY PICTURE '9999'

      * SUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,67  SAY PRICE     PICTURE '9999.99'
      @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW+1
      SELE INVLINE
      IF llLineUp
         EXIT
      ENDIF
      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING
   SET DEVICE TO PRINT
   IF llLineUp  
      EJECT
      lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
      DO CASE
         CASE lnChoice = 3
           RETURN(.F.)
        CASE lnChoice = 2
           llLineUp =.F.
      ENDCASE
      WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
      SET DEVICE TO PRINT
      STORE 0.00 TO XPIECES, XSUBTOTAL
      LOOP
   ENDIF
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1'
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      @ 50,55 SAY 'MERCHANDISE'
      @ 50,75 SAY '******'
      ROW=ROW+1
      @ 52,07 SAY INVHDR.NOTE1
      @ 53,07 SAY INVHDR.NOTE2
      LOOP
   ENDIF
   SELECT INVHDR
   @ 50,55 SAY 'MERCHANDISE'
   @ 50,75 SAY XSUBTOTAL     PICTURE '999999.99'
   ROW=ROW+1
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ 51,55 SAY 'TOTAL - FREIGHT'
      @ 51,75 SAY WKAMT                PICTURE '999999.99'
      ROW=ROW+1
   ENDIF
   @ 52,07 SAY INVHDR.NOTE1
   IF DISCOUNT<>0
      @ 52,55 SAY 'DISCOUNT'
      @ 52,75 SAY DISCOUNT    PICTURE  '999999.99'
   ENDIF
   @ 53,07 SAY INVHDR.NOTE2
   @ 57,12 SAY INVHDR.CARTONS     PICTURE '@Z 999'
   @ 57,64 SAY XPIECES            PICTURE '9999999'
   @ 57,75 SAY INVHDR.TOTALCHG    PICTURE '999999.99'
   * GET NEXT HEADER RECORD
   SELECT &INVHTEMP
   SKIP
   NEWDOC = .T.
ENDDO
SET DEVICE TO SCREEN
RETURN