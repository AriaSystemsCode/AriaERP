*:***************************************************************************
*: Program file  : ARPINVME.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR MELINDA ENG INC.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101150)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 04/29/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102303
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVME
*:***************************************************************************
           *--Section of Initializing the variables --*
llNote_Loop = .F.        && Flag to indicate whether we have finished printing the Notepad or not.
LnNotLine   = 1          && For the notepad
llNoRec     = .F.        && Variable used to make the endreport function in the main program.
llNewDoc    = .T.
lnMaxRow    = 54
STORE  0   TO lnSubTot,lnPage
STORE .F.  TO llEndPage

               *--Section to get the Style/Color Lengths --*
*--THE COLOR LENGTH
STORE 0 TO lnClrLnMl , lnClrPosMl
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnMl  = LEN(laItemSeg[lnCount,3])
    lnClrPosMl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

               *--End Section to get the Style/Color Lengths --*

           *--Section of Opening the files doesn't found in the OG --*
lcOdQtyTmp = GFTEMPNAME()
lcConsNord = GFTEMPNAME()
lcSalsRp = GFTEMPNAME()
IF !USED(lcSalsRp)
  =gfOpenFile(gcDataDir+"SALESREP","SALESREP", 'SH', @lcSalsRp, .T.)
ENDIF  

lcOrdLine = GFTEMPNAME()
IF !USED(lcOrdLine)
  =gfOpenFile(gcDataDir+"Ordline","Ordline", 'SH', @lcOrdLine, .T.)
ENDIF  


SELECT INVHDR
LOCATE FOR &lcRpExp

SELECT INVHDR
SET RELATION TO REP1 INTO (lcSalsRp) ADDITIVE

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
  lcinvoce = INVOICE
  *- Get the header Info.   
  IF llNewDoc
    STORE 0.00 TO lnSubTot

    lnPage    = 0
    llEndPage = .F.
    llNewDoc  = .F.
    lcOrder   = ORDER
    lcNote1   = IIF(NOTE1<>'*', NOTE1, '')
    lcNote2   = IIF(NOTE2<>'*', NOTE2, '')
    lcStore   = STORE
    lcAccount = ACCOUNT

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    =SEEK(IIF(lcStore = SPACE(8),'M'+lcAccount,'S'+lcAccount+lcStore))
    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.

    lcBtName  = lcSolTName
    lcBtAddr1 = laSoldTo[1]
    lcBtAddr2 = laSoldTo[2]
    lcBtAddr3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
      lcBtAddr2 = laSoldTo[3]
      lcBtAddr3 = ''
    ENDIF

    lcStName  = lcShpTName
    lcStAddr1 = laShipTo[1]
    lcStAddr2 = laShipTo[2]
    lcStAddr3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      lcStAddr2 = laShipTo[3]
      lcStAddr3 = ''
    ENDIF

    *--Get the Descreption about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerms   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    lcShipVia = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')

  ENDIF
  *-----------------
  * START PRINT
  *-----------------
  =lfPrintHdr()

  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  ROW = 29
  IF INVHDR.CONSOL = 'Y'
    *--Index on STYLE+COLOR+INVOICE+STR(LINENO,6)
    SET ORDER TO TAG INVLINES IN INVLINE

    IF EMPTY(lcOrder)
      =lfCretTmp()
      SELECT CONSINVH
      =SEEK(lcinvoce)
      SCAN REST WHILE INVOICE + STORE + ORDER = lcinvoce
        lcOrder = CONSINVH.ORDER
        SELECT STYLE,SCALE,SUM(QTY1) AS nQTY1,SUM(QTY2) AS nQTY2,;
               SUM(QTY3) AS nQTY3,SUM(QTY4) AS nQTY4 ,SUM(QTY5) AS nQTY5,;
               SUM(QTY6) AS nQTY6,SUM(QTY7) AS nQTY7,SUM(QTY8) AS nQTY8;
              FROM (lcOrdLine) WHERE cordtype+ORDER+STR(LINENO,6) = "O" + lcOrder ;
              GROUP BY STYLE INTO TABLE (lcConsNord)

       SELECT (lcOdQtyTmp)
       APPEND FROM (lcConsNord)

      ENDSCAN
      SELECT (lcOdQtyTmp)
      LOCATE
    ENDIF

    IF !EMPTY(INVHDR.ORDER)
      IF USED(lcOdQtyTmp)
        USE IN (lcOdQtyTmp)
      ENDIF
      
      SELECT STYLE,SCALE,SUM(QTY1) AS nQTY1,SUM(QTY2) AS nQTY2,;
             SUM(QTY3) AS nQTY3,SUM(QTY4) AS nQTY4 ,SUM(QTY5) AS nQTY5,;
             SUM(QTY6) AS nQTY6,SUM(QTY7) AS nQTY7,SUM(QTY8) AS nQTY8;
            FROM (lcOrdLine) WHERE cordtype+ORDER+STR(LINENO,6) = "O" + lcOrder ;
            GROUP BY STYLE INTO TABLE (lcOdQtyTmp)
    ENDIF

    *-- Set a relation into the style file.
    IF !('STYLE INTO STYLE' $ SET('RELATION'))
      SET RELATION TO STYLE INTO STYLE ADDITIVE
    ENDIF

    SCAN WHILE ROW <= lnMaxRow
      *--Get this record in the INVLINE file.
      =SEEK(STYLE + INVHDR.INVOICE , 'INVLINE')
      =SEEK("S" + INVLINE.SCALE ,'SCALE')
      FOR lnSz = 1 TO SCALE.CNT
        lcSz   = STR(lnSz,1)
        IF INVLINE.QTY&lcSz + nQty&lcSz > 0
          @ ROW,03 SAY INVLINE.QTY&lcSz + nQty&lcSz             PICTURE '999999'
          @ ROW,09 SAY INVLINE.QTY&lcSz                         PICTURE '999999'
          @ ROW,15 SAY nQty&lcSz                                PICTURE '999999'
          @ ROW,22 SAY SUBSTR(STYLE,1,7)+'-'+SUBSTR(STYLE,lnClrPosMl,3)
          @ ROW,35 SAY STYLE.DESC + '  ' +SCALE.SZ&lcSz
          @ ROW,65 SAY INVLINE.PRICE                            PICTURE '9999.99'
          @ ROW,74 SAY INVLINE.PRICE*INVLINE.QTY&lcSz           PICTURE '999999.99'
          lnSubTot = lnSubTot +INVLINE.PRICE*INVLINE.QTY&lcSz
          ROW = ROW + 1

          IF ROW = lnMaxRow
            llEndPage = IIF(INVHDR.CONSOL = 'Y',!EOF(lcOdQtyTmp),cordtype+ORDER+STORE = "O"+lcOrder+lcStore)
            IF llEndPage .AND. !llNote_Loop
              @ ROW+1,12 SAY 'C O N T I N U E D ...'
              =lfPrintHdr()
              SELECT (lcOdQtyTmp)
            ENDIF
          ENDIF

        ENDIF
      ENDFOR
    ENDSCAN
  ELSE
    *--Non cosolidated invoice.
    SELECT (lcOrdLine)
    IF !llEndPage
      SET ORDER TO TAG ORDLINST
      =SEEK("O" + lcOrder + lcStore)

      *--Index on ORDER+STR(LINENO,6)+INVOICE
      SET ORDER TO TAG INVLINEO IN INVLINE
    ENDIF

    SCAN WHILE cordtype+ORDER+STORE+STYLE+STR(LINENO,6) = "O"+lcOrder+lcStore AND ROW <= lnMaxRow
      =SEEK(lcOrder+STR(LINENO,6)+lcinvoce,'INVLINE')
      =SEEK("S" + INVLINE.SCALE ,'SCALE')
      FOR lnSz = 1 TO SCALE.CNT
        lcSz   = STR(lnSz,1)
        IF INVLINE.QTY&lcSz+QTY&lcSz > 0
          @ ROW,03 SAY INVLINE.QTY&lcSz+QTY&lcSz                  PICTURE '999999'
          @ ROW,09 SAY INVLINE.QTY&lcSz                           PICTURE '999999'
          @ ROW,15 SAY QTY&lcSz                                   PICTURE '999999'
          @ ROW,22 SAY SUBSTR(STYLE,1,7)+'-'+SUBSTR(STYLE,lnClrPosMl,3)
          @ ROW,35 SAY STYLE.DESC + '  ' +SCALE.SZ&lcSz
          @ ROW,65 SAY INVLINE.PRICE                              PICTURE '9999.99'
          @ ROW,74 SAY INVLINE.PRICE*INVLINE.QTY&lcSz             PICTURE '999999.99'
          lnSubTot = lnSubTot +INVLINE.PRICE*INVLINE.QTY&lcSz
          ROW = ROW + 1

          IF ROW = lnMaxRow
            llEndPage = IIF(INVHDR.CONSOL = 'Y',!EOF(lcOdQtyTmp),cordtype+ORDER+STORE = "O"+lcOrder+lcStore)
            IF llEndPage .AND. !llNote_Loop
              @ ROW+1,12 SAY 'C O N T I N U E D ...'
              =lfPrintHdr()
              SELECT (lcOrdLine)
            ENDIF
          ENDIF

        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF

  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + lcinvoce
      SEEK 'C' + lcinvoce
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + lcinvoce
      @ ROW,04 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= lnMaxRow
          llNote_Loop = .T.
          lnNotLine = lnNotLine - 1
        ELSE
          llNote_Loop = .F.
          @ ROW,04 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= lnMaxRow
          =lfGetcont()
          =lfPrintHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !llNote_Loop
        @ ROW,04 SAY '* -- END OF NOTES -- *'
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
  ROW = IIF(!llNote_Loop,ROW,ROW + 1)
  IF ROW >55
    ROW = 30
    =lfPrintHdr()
  ELSE
    ROW  = 55
  ENDIF

  @ ROW,04 SAY '** NO CLAIMS ALLOWED UNLESS MADE'
  @ ROW,54 SAY 'Discount'
  @ ROW,72 SAY DISCOUNT         PICTURE '99999999.99'
  ROW=ROW+1

  @ ROW,04 SAY '   IMMEDIATELY UPON RECEIPT OF GOODS.'
  @ ROW,54 SAY 'Subtotal'
  @ ROW,72 SAY lnSubTot + Discount PICTURE '99999999.99'
  ROW=ROW+1

  @ ROW,04 SAY '** NO RETURN WILL BE ACCEPTED WITHOUT'
  ROW=ROW+1

  @ ROW,04 SAY '   PRIOR AUTHORIZATION.'
  @ ROW,54 SAY 'Insurance'
  @ ROW,73 SAY INSUR     PICTURE '9999999.99'   
  ROW=ROW+1

  @ ROW,04 SAY '** SERVICE CHARGE OF 1.5 % PER MONTH'
  ROW=ROW+1

  @ ROW,04 SAY '   WILL BE ADDED TO ALL PAST DUE ACCOUNTS.'
  @ ROW,54 SAY 'Shipping & Handling'   
  @ ROW,73 SAY FREIGHT+COD   PICTURE '9999999.99'   
  ROW=ROW+1

  @ ROW,04 SAY '** PLEASE DO NOT DEDUCT FREIGHT CHARGE.'
  @ ROW,54 SAY 'TOTAL'
  lnTotal = Discount+lnSubTot+insur+Freight+Cod
  @ ROW,72 SAY lnTotal   PICTURE '99999999.99'
  ROW=ROW+1

  @ ROW,04 SAY '   WE WILL BILL YOU IF DEDUCTED.'

  SELECT INVHDR
  IF EOF()
    llNewDoc = .F.
    SET DEVICE TO SCREEN
    RETURN
  ELSE
     llNewDoc = .T.
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
*! Name      : lfPrintHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/269/2001
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : ARPINVME.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrintHdr()
*!*************************************************************
FUNCTION lfPrintHdr

SELECT INVHDR
lnPage = lnPage +1
@ 03,05 SAY IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
@ 03,70 SAY lcinvoce

@ 04,05 SAY TRIM(laCompAdd[1])
@ 05,05 SAY IIF(!EMPTY(TRIM(laCompAdd[2])),UPPER(TRIM(laCompAdd[2])),TRIM(laCompAdd[3]))
@ 05,70 SAY lcAccount
@ 06,05 SAY IIF(!EMPTY(TRIM(laCompAdd[2])),TRIM(laCompAdd[3]),'')
@ 07,70 SAY "Page # :" 
@ 07,78 SAY lnPage PICTURE '9999'

@ 13,49 SAY IIF(!EMPTY(lcStore),"Store # " +lcStore,"")
@ 14,06 SAY lcBtName
@ 14,49 SAY lcStName

@ 15,06 SAY lcBtAddr1
@ 15,49 SAY lcStAddr1

@ 16,06 SAY lcBtAddr2
@ 16,49 SAY lcStAddr2

@ 17,06 SAY lcBtAddr3
@ 17,49 SAY lcStAddr3
@ 18,49 SAY lcNote1
@ 19,49 SAY lcNote2

@ 24,04 SAY INVDATE
@ 24,15 SAY LEFT(lcShipVia,15)
@ 24,52 SAY LEFT(lcTerms,14)
@ 26,04 SAY IIF(OrdHdr.MultiPO,'*MULTI*', LEFT(CUSTPO,10))
@ 26,23 SAY ORDHDR.START
@ 26,32 SAY REP1 +' '+ &lcSalsRp..NAME
@ 26,69 SAY ORDER

ROW = 29
*--End of lfPrintHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/2001
*! Purpose   : PRINT THE FOOTER OF THE INVOICE.
*!*************************************************************
*! Called from : ARPINVME.PRG
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

*--End of lfGetcont.
*!*************************************************************
*! Name      : lfCretTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/29/2001
*! Purpose   : To creat tmp file in case multi sales order.
*!*************************************************************
*! Called from : ARPINVME.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfCretTmp

llFlag1 = .T.
llFlag2 = .T.

IF !USED(lcOdQtyTmp)
  CREATE Table (lcOdQtyTmp) (STYLE C(19) , SCALE C(3)  , nQTY1 N(6) , nQTY2 N(6) ,;
                             nQTY3 N(6)  , nQTY4 N(6)  , nQTY5 N(6) , nQTY6 N(6) ,;
                             nQTY7 N(6)  , nQTY8 N(6))
  ZAP
  INDEX ON STYLE TAG STYLE OF (gcWorkDir+lcOdQtyTmp)
  llFlag1 = .F.
ENDIF

IF !USED(lcConsNord)
  CREATE Table (lcConsNord) (STYLE C(19) , SCALE C(3)  , nQTY1 N(6) , nQTY2 N(6) ,;
                             nQTY3 N(6)  , nQTY4 N(6)  , nQTY5 N(6) , nQTY6 N(6) ,;
                             nQTY7 N(6)  , nQTY8 N(6))
  ZAP
  INDEX ON STYLE TAG STYLE OF (gcWorkDir+lcConsNord)
  llFlag2 = .T.
ENDIF

IF llFlag1
  SELECT (lcOdQtyTmp)
  ZAP
ENDIF

IF llFlag2
  SELECT (lcConsNord)
  ZAP
ENDIF

*--End of lfCretTmp.