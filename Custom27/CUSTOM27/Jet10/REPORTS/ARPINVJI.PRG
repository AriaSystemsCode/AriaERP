*:***************************************************************************
*: Program file  : ARPINVJI.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR JEETISH IMPORTS INC.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 12/05/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C102066
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVJI
*:***************************************************************************
           *--Section of opening the nessessry files--*
IF !USED(lcOrdLine)
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

           *--Section of Initializing the variables --*
XSTYNLOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine = 1        && For the notepad
llNoRec   = .F.      && Variable used to make the endreport function in the main program.
NEWDOC    = .T.
MAXROW    = 52

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
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC = .F.
    XORDER = ORDER
    XPHONE    = INVHDR.PHONE
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

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

    *-- GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
    lcDvsion   = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
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

    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN

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
  XSTORE = STORE
  XSCALE =  ' '
  ROW    = 32

  DO WHILE CURLOOP = '1' .AND. !XSTYNLOOP
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

    SELECT (lcOrdLine)
    =SEEK("O" + INVLINE.ORDER +STR(INVLINE.LINENO,6), lcOrdLine )
    IF FOUND()
      MSTYLE = LEFT(STYLE,12)
      MCOLOR = SUBSTR(STYLE,14,6)
    ELSE
      MSTYLE = LEFT(INVLINE.STYLE,12)
      MCOLOR = SUBSTR(INVLINE.STYLE,14,6)
    ENDIF

    SELECT INVLINE
    @ ROW,02 SAY SUBSTR(MSTYLE,1,7)
    IF LEFT(INVLINE.STYLE,12) <> MSTYLE
      @ROW,9 SAY '/'+SUBSTR(STYLE,1,7)
    ENDIF
    @ ROW,17 SAY SUBSTR(MCOLOR,1,3)
    IF SUBSTR(INVLINE.STYLE,14,6) <> MCOLOR
      @ROW,21 SAY '/'+SUBSTR(COLOR,1,3)
    ENDIF

    @ ROW,30 SAY STYLE.DESC
    @ ROW,58 SAY TOTQTY PICTURE '99999'

    * SUM INVOICE TOTALS
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT

    @ ROW,67  SAY PRICE     PICTURE '9999.99'
    @ ROW,75  SAY XLINETOT  PICTURE '999999.99'

    ROW = ROW+1

    * GET NEXT LINE ITEM
    SELECT INVLINE
    SKIP
  ENDDO

  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' AND !XSTYNLOOP
    =lfGetcont()
    LOOP
  ENDIF

  SELECT INVHDR
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,58 SAY XPIECES              PICTURE '999999'
  @ ROW,75 SAY XSUBTOTAL            PICTURE '999999.99'
  ROW=ROW+1

  *----------------------------------------------------
  * [NOTEPAD] COMMENT LINES
  *----------------------------------------------------

  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + XINVOICE
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XSTYNLOOP = .T.
        ELSE
          XSTYNLOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XSTYNLOOP
        lnNotLine = 1
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF


  SELECT INVHDR
  @ 55,02 SAY XPHONE + ' ' + XNOTE1 + ' ' + XNOTE2
  IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
    @ 57,10 SAY lcRpMsg1
    @ 58,10 SAY lcRpMsg2
  ENDIF
  ROW = 58
  IF DISCOUNT<>0
    @ ROW,58 SAY 'TOTAL DISCOUNT'
    @ ROW,75 SAY DISCOUNT             PICTURE '999999.99'
    ROW=ROW+1
  ENDIF

  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    @ ROW,58 SAY 'TOTAL FREIGHT'
    @ ROW,75 SAY WKAMT                PICTURE '999999.99'
    ROW=ROW+1
  ENDIF
  @ 62,09 SAY CARTONS                 PICTURE '@Z 999'
  @ 62,75 SAY INVHDR.TOTALCHG         PICTURE '999999.99'

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
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 12/05/2000
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
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 12/05/2000
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
@ 05,60 SAY XINVOICE
@ 05,74 SAY INVDATE
@ 07,74 SAY lcDvsion
@ 09,74 SAY ORDER
@ 11,74 SAY PIKTKT
@ 13,74 SAY IIF(INVHDR.APPROVAL <> ' ' .AND. UPPER(INVHDR.APPROVAL) <> 'DEC' , INVHDR.APPROVAL , "")
@ 17,9  SAY XBTNAME

IF CURLOOP = '1'
  @ 17,57 SAY XSTNAME
ENDIF

@ 18,9  SAY XBTADDR1
IF CURLOOP = '1'
  @ 18,57 SAY XSTADDR1
ENDIF

@ 19,9  SAY XBTADDR2
IF CURLOOP = '1'
  @ 19,57 SAY XSTADDR2
ENDIF

@ 20,9  SAY XBTADDR3
IF CURLOOP = '1'
  @ 20,57 SAY XSTADDR3
ENDIF

@ 25,02 SAY ACCOUNT
@ 25,14 SAY LEFT(CUSTPO,10)
@ 25,24 SAY IIF(CURLOOP='1',XSTORE,'')
@ 25,34 SAY DEPT
@ 25,41 SAY LEFT(PTERMS,15)
@ 25,64 SAY REP1
@ 25,68 SAY REP2
@ 25,72 SAY PSHIPVIA

ROW    = 32
*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 12/05/2000
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
@ 55,02 SAY XPHONE + ' ' + INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 56,10 SAY lcRpMsg1
  @ 57,10 SAY lcRpMsg2
ENDIF
@ 62,09 SAY '***'
@ 62,67 SAY '*****'
@ 62,75 SAY '******.**'

*-- End Of lfGetcont.