*:**************************************************************************
*: Program file  : SOORCNC.PRG
*: Program desc. : Order Confirmation Form C (Standard Form)
*: System        : Aria Advantage Series.
*: Module        : Sales Order Module (SO)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: References    : E301517,1
*:**************************************************************************
*: Functions     : lfPrntNote.
*:**************************************************************************
*: Modifications:
*:E301517,4 MHM  02/01/2001 FIx Some Bugs Came From Testing 
*:B804467,1 MHM  10/25/2001 FIx the bug of total merchandise in multi page order Not correct
*:B804441,1 MHM 10/25/2001 Fix bug of wrong ShipVia in Multi Store Order.  
*:E301939,1 RAE 09/05/2002 Add an option filter - Print Book Qty  (Y/N) - to print booked 
*                         quantity for partially shipped orders.
*:B606654,1 BWA 01/23/2003 Add the condition of the decimal to the code.
*:B607321,1 KHM 06/02/2003 Fix the problem of not displaying the terms description
*:B607321,1                and the not printing the sold to address correctly.
*:C126257,1 NNA 02/12/2005 Display OrdHdr.cAddress5 in a Separate Line
*:**************************************************************************

PRIVATE ROW , lnNoteLn , lnSvNoteLn , llNewDoc , llNormPrnt , lcPrvScale , ;
        lnTotQty , lnTotOrd , lcOrder , lcOldStore ,  llNotePrnt , ;
        lnMemoLine , lcMajTitle , lcNonMajTl , lcScales
*E301939,1 RAE Declare variable to hold the total Book Qty. [start]
PRIVATE lnTotBok
*E301939,1 RAE [end]

STORE 1 TO lnNoteLn,lnSvNoteLn
STORE .T. TO llNewDoc , llNormPrnt
STORE '' TO lcOldStore , lcScales , lcMulti 
*:E301517,4 MHM  02/01/2001 initialize total quantity here [Start]

*:B804467,1 MHM  10/25/2001 initialize total amount here[start]
*STORE 0 TO lnTotQty 
*E301939,1 RAE [start]
*STORE 0 TO lnTotQty ,lnTotOrd
STORE 0 TO lnTotQty , lnTotOrd , lnTotBok
*E301939,1 RAE [end]
*:B804467,1 MHM  [End]

STORE "" TO lcBtName
llNoteYes = .F.
*--*:E301517,4 MHM  02/01/2001 [End]
llNoRec = .F.
lcMajTitle = ALLTRIM(gfItemMask('HM'))      && get Major title
lcNonMajTl = ALLTRIM(gfItemMask('HN'))      && get Non Major title

*--*:E301517,4 MHM  02/01/2001 [Start]
SELECT OrdHdr
SET SKIP TO
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  llNoRec = .T.
  RETURN
ENDIF
*--*:E301517,4 MHM  02/01/2001 [End]
SELECT OrdHdr
SET RELATION OFF INTO (lcTempOrd)
SELECT (lcTempOrd)
LOCATE
SET DEVICE TO PRINT
DO WHILE INKEY() <> 32
  SELECT OrdHdr
  
  IF EOF(lcTempOrd)
    IF llNewDoc
      EXIT
    ENDIF  
  ELSE 
    SEEK 'O' + &lcTempOrd..Order
    *E301517,4 MHM  02/01/2001[start]
    IF lcMulTi = 'Y' 
      =SEEK('S'+ &lcTempOrd..Account+ &lcTempOrd..Store,'CUSTOMER')
    ENDIF
    *E301517,4 MHM  02/01/2001[end]
  ENDIF  

  IF FLAG = 'N'
    SKIP
    IF SEEK ('O' + Order,lcTempOrd)
      LOOP
    ENDIF  
  ENDIF
  *:E301517,4 MHM  02/01/2001  move lntotqty to initialize in the header     [start]
  *STORE 0 TO lnTotQty ,lnTotOrd

  *:B804467,1 MHM  10/25/2001 move lnTotOrd to initialize in the header[start]
  *STORE 0 TO lnTotOrd
  *:B804467,1 MHM  [End]

  *:E301517,4 MHM  02/01/2001 [End]
  IF llNewDoc
    *:E301517,4 MHM to get address [Start]
    =lfGetOdHdr()
    *=lfSolSpAdr()
    *E301517,4 MHM [end]
    lcOrder    = Order
    lcMulTi    = MulTi
    llNotePrnt = .F.
  ENDIF

  *:E301517,4 MHM to get address [Start]
  *DO lpPrintHdr
  IF llRpOrdNot AND llNoteYes         
    lnalase = ALIAS()
    SELECT(lcTempOrd)
    SKIP-1
    =lfGetOdHdr()
    SELECT(lnalase)
  ENDIF  
  DO lpPrintHdr
  IF llRpOrdNot AND llNoteYes
    lnalase = ALIAS()
    SELECT(lcTempOrd)
    SKIP
    SELECT(lnalase)
    llNoteYes = .F.
  ENDIF  
  *:E301517,4 MHM to get address [End]
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
*123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
*            11111 22222 33333 44444 55555 66666 77777 88888
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

  *-- Print Order line notes.
  SELECT (lcTempOrd)
  ROW        = 22
  llNewDoc   = .T.
  lcPrvScale = ''

  IF !(Store == lcOldStore) AND !llNotePrnt
    lcOldStore = Store
  ENDIF  

  DO WHILE .T.
    DO CASE
      CASE EOF() OR Order <> lcOrder
        llNewDoc = .T.
        EXIT
      CASE Store <> lcOldStore AND lcMulTi = 'Y'
        *E301517,4 MHM to get address [Start]
        IF llRpOrdNot AND llNoteYes         
          SKIP-1
        ENDIF  
        =lfGetOdHdr()
        IF llRpOrdNot AND llNoteYes
          SKIP
          llNoteYes = .F.
        ENDIF  
        *=lfSolSpAdr()
        *E301517,4 MHM [end]
        llNewDoc = .F.
        EXIT
      CASE TotQty <= 0
        *E301939,1 RAE skip TOTQTY = 0 record if Print Book Qty is 'No'.[start]
        *SKIP
        *LOOP
        IF lcRpBook = 'N'
          SKIP
          LOOP
        ENDIF
        *E301939,1 RAE [end]        
      CASE ROW >= 51
        llNewDoc = .F.
        EXIT
    ENDCASE

    IF llNormPrnt
      @ ROW,00 SAY LineNo
      @ ROW,07 SAY Group
      @ ROW,10 SAY Style
      @ ROW,30 SAY Style.Desc

      *B606654,1 BWA 01/23/2003 Add the condition of the decimel to the code.[START]
      *@ ROW,51 SAY Price PICTURE '9999.99'
      *lnAmount = TotQty * Price
      *@ ROW,69 SAY lnAmount  PICTURE '999,999.99'

      IF lcRpDeciml = "Y"
        @ ROW,51 SAY Price PICTURE '9999.99'
        lnAmount = TotQty * Price
        @ ROW,69 SAY lnAmount  PICTURE '999,999.99'
      ELSE
        @ ROW,51 SAY Price PICTURE '9999999'
        lnAmount = TotQty * Price
        @ ROW,69 SAY lnAmount  PICTURE '99,999,999'
      ENDIF
      *B606654,1 BWA 01/23/2003.[END]

      ROW = ROW + 1
   
      IF !(Scale == lcPrvScale)
        PRIVATE lcScales
        lcScales   = ''
        lcPrvScale = Scale
        *XSCALE   = GETSCALE(Style.Scale,SPACE(1) )
        IF Scale.Cnt >=1        
          FOR lnX = 1 TO Scale.Cnt
            lnZ = STR(lnX,1)

            *B606654,1 BWA 01/23/2003 Print in the right postion.[START]
            *lcScales = lcScales + Scale.Sz&lnz + ' '
            lcScales = lcScales + PADL(ALLTRIM(Scale.Sz&lnz) , 5 , " ") + ' '
            *B606654,1 BWA 01/23/2003.[END]

            *:E301517,4 MHM  02/01/2001 no need to add counter for loop raise it [Start]
            *lnX = lnX + 1
            *:E301517,4 MHM  02/01/2001 [End]
          ENDFOR        
          @ ROW,11 SAY lcScales
          ROW = ROW+1
        ENDIF
      ENDIF

      @ ROW,05 SAY 'ORDER:'
      lnX      = 1
      lnColumn = 11
      DO WHILE Scale.Cnt >= 1 AND lnX <= 8
        lnZ =STR(lnX,1)
        @ ROW,lnColumn SAY Qty&lnZ PICTURE '@Z 99999'
        lnX = lnX + 1
        lnColumn = lnColumn + 6
      ENDDO
      @ ROW,60 SAY TotQty PICTURE '999999'
      ROW = ROW + 1
      *E301939,1 RAE print Book Qty1 to Qty8 [start]
      IF lcRpBook = 'Y'
        @ ROW,05 SAY 'BOOK:'
        lnX      = 1
        lnColumn = 11
        DO WHILE Scale.Cnt >= 1 AND lnX <= 8
          lnZ =STR(lnX,1)
          @ ROW,lnColumn SAY book&lnZ PICTURE '@Z 99999'
          lnX = lnX + 1
          lnColumn = lnColumn + 6
        ENDDO
        @ ROW,60 SAY TotBook PICTURE '999999'
        ROW = ROW + 1
      ENDIF
      *E301939,1 RAE [end]      
      @ ROW,05 SAY 'PICK: _____ _____ _____ _____ _____ _____ _____ _____   _____' 
      ROW = ROW + 2

      lnTotQty  = lnTotQty + TotQty
      lnTotOrd  = lnTotOrd + lnAmount
      *E301939,1 RAE [start]
      lnTotBok = lnTotBok + TotBook
      *E301939,1 RAE [end]
    ENDIF

    IF llRpOrdLnt AND MEMLINES(Note_Mem) <> 0 AND !lfPrntNote()
      llNormPrnt = .F.
      EXIT
    ELSE
      llNormPrnt = .T.
    ENDIF
    ROW = ROW + 1
    SKIP
  ENDDO
   
  *-- Print NotePad
  *-- If Print Order Notes (Yes) and Note not printed yet
  IF llRpOrdNot AND !llNotePrnt 
    *-- If new Order or (Multi store and Store is changed)
    IF llNewDoc OR (lcMulTi = 'Y' AND !llNewDoc AND lcOldStore <> Store)
      SELECT NotePad
      SEEK 'B' + lcOrder
      llNotePrnt = IIF(FOUND(), .T., .F.)    
      lnMemoLine = 1
    ENDIF  
    *-- EndIf of new Order or (Multi store and Store is changed)
  ENDIF
  *-- EndIf of Print Order Notes (Yes) and Note not printed yet

  *-- Notepad comment lines
  IF llRpOrdNot
    PRIVATE lnMemWidth , lnMTotLin
    lnMemWidth = SET("MEMOWIDTH")           && Save MemoWidth Settings.
    SET MEMOWIDTH TO 75                 
    lnMTotLin = MEMLINES(NotePad.MNOTES)

    *-- Loop around NotePad lines to print it line by line
    DO WHILE llNotePrnt
      SELECT NotePad
      IF lnMemoLine > lnMTotLin
        IF lcMulTi = 'Y' AND lcOrder = &lcTempOrd..Order
          llNewDoc = .F.
        ELSE
          llNewDoc = .T.
        ENDIF
        llNotePrnt = .F.
        EXIT
      ENDIF
      IF ROW >= 51
        llNewDoc = .F.
        llNotePrnt = IIF(lnMemoLine = lnMTotLin,.F.,.T.)   
        *:E301517,4 MHM [Start]
        llNoteYes = .T.
       *:E301517,4 MHM [End]
        EXIT
      ENDIF
      
      IF  '*' <> LEFT(MLINE(MNOTES,lnMemoLine),1)  
        @ ROW,04 SAY MLINE(MNOTES,lnMemoLine)
        ROW = ROW+1
      ENDIF
      lnMemoLine = lnMemoLine + 1
    ENDDO
    *-- EndLoop around NotePad lines to print it line by line

    SET MEMOWIDTH TO lnMemWidth             && Restore MemoWidth Settings.
  ENDIF


  *-- If (Multi Store and same Store and same order) OR Note is printed
  *-- Not Multi Store and same Order
  IF ((lcMulTi = 'Y' AND lcOldStore = Store AND !llNewDoc ) OR llNotePrnt) OR ;
     lcMulTi <> 'Y' AND !llNewDoc
    @ 53,00 SAY REPLICATE('-',80)
    @ 54,13 SAY 'C O N T I N U E D ...'
    @ 54,69    SAY '**********'
    @ 55,00 SAY REPLICATE('-',80)
    IF !EOF(lcTempOrd)
      LOOP
    ENDIF  
  *:E301517,4 MHM  02/01/2001 if not contanued then prent Footer [Start]
  *ENDIF
  ELSE
  *:E301517,4 MHM  02/01/2001 [End]    
    *E301939,1 RAE print TOTAL PIECES FOR BOOK QTY. [start]
    *ROW = 56
    *@ ROW,44 SAY 'TOTAL PIECES==>'
    *@ ROW,60 SAY lnTotQty PICTURE '9999999'
    IF lcRpBook = 'Y'
      ROW = 55
      @ ROW,24 SAY 'TOTAL OPEN PIECES==>'
      @ ROW,58 SAY lnTotQty PICTURE '9999999'
        
      ROW = 56
      @ ROW,24 SAY 'TOTAL BOOKED PIECES==>'
      @ ROW,58 SAY lnTotBok PICTURE '9999999'       
    ELSE
      ROW = 56
      @ ROW,44 SAY 'TOTAL PIECES==>'
      @ ROW,60 SAY lnTotQty PICTURE '9999999'      
    ENDIF
    *E301939,1 RAE [end]
    ROW = 57
    @ ROW,01 SAY REPLICATE('*',80)
    @ ROW+1,01 SAY '| REP |  APPROVAL | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'
    ROW =ROW + 2 
    @ ROW,01 SAY '|'
    @ ROW,03 SAY OrdHdr.Rep1
    @ ROW,07 SAY '|'
    @ ROW,09 SAY OrdHdr.Approval
    @ ROW,19 SAY '|           |       |           |           |'
    *:E301517,4 MHM  02/01/2001 [Start]
    *@ ROW,64 SAY lcShipVia
    @ ROW,64 SAY LEFT(lcShipVia,15)
    *:E301517,4 MHM  02/01/2001 [End]
    @ ROW,79 SAY '|'
    @ ROW+1,1 SAY REPLICATE('*',80)
    @ ROW+2,1 SAY '|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS|  TERMS    | COMMENTS:     |'
    ROW = ROW + 3
    @ ROW,01 SAY '|  $'

    *B606654,1 BWA 01/23/2003 Add the condition of the decimel to the code.[START]
    *@ ROW,05 SAY lnTotOrd PICTURE '9,999,999.99'
    IF lcRpDeciml = "Y"
      @ ROW,05 SAY lnTotOrd PICTURE '9,999,999.99'
    ELSE
      @ ROW,05 SAY lnTotOrd PICTURE '999,999,999'
    ENDIF
    *B606654,1 BWA 01/23/2003.[END]

    @ ROW,19 SAY '| $         | $     | $         |'
    *:E301517,4 MHM  02/01/2001 [Start]
    *@ ROW,52 SAY lcTerms
    @ ROW,52 SAY LEFT(lcTerms,10)
    *:E301517,4 MHM  02/01/2001 [End]
    @ ROW,63 SAY '|' 
          
    IF SUBSTR(OrdHdr.Note1,1,1) <> '*'      
      @ ROW,64  SAY SUBSTR(OrdHdr.Note1,1,15)    
    ENDIF  
    @ ROW,79 SAY '|' 
    @ ROW+1,01 SAY REPLICATE('*',80)
    *:E301517,4 MHM  02/01/2001 [Start]
    lnTotQty  = 0.00
    lnTotORd  = 0.00
    *E301939,1 RAE [start]
    lnTotBok  = 0.00
    *E301939,1 RAE [end]
  ENDIF    
  *:E301517,4 MHM  02/01/2001 [End]
ENDDO
SET DEVICE TO SCREEN
RETURN
*-- End of Report.

*!**************************************************************************
*! Name      : lfPrntNote
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2000
*! Purpose   : Print Order line notes.
*!**************************************************************************
*! Example   : = lfPrntNote()
*!**************************************************************************
*
FUNCTION lfPrntNote
ROW = ROW - 1
lnNoteLn = lnSvNoteLn
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= 50
    llNewDoc   = .F.
    lnSvNoteLn = lnNoteLn
    RETURN .F.
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
lnSvNoteLn = 1
RETURN .T.
*-- End of lfPrntNote.

*!**************************************************************************
*! Name      : lpPrintHdr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/24/2000
*! Purpose   : Print Header information.
*!**************************************************************************
*! Example   : DO lpPrintHdr
*!**************************************************************************
*
PROCEDURE lpPrintHdr
*-- START PRINTING
@ 01,01 SAY 'P I C K I N G  T I C K E T'
*:E301517,4 MHM nothing called lcSolTName [Start]
*@ 01,35 SAY lcSolTName
@ 01,35 SAY lcBtName
*:E301517,4 MHM [End]
@ 01,70 SAY DATE()
@ 02,01 SAY lcCompName
@ 02,35 SAY laSoldTo[1]
@ 03,01 SAY laCompAdd[1]
@ 04,01 SAY laCompAdd[2]
@ 04,50 SAY DATE()
@ 05,01 SAY laCompAdd[3]
@ 05,35 SAY 'ORDER:'
@ 05,42 SAY lcOrder
@ 05,50 SAY Start
@ 05,60 SAY Complete
@ 06,01 SAY lcCompPhon PICTURE lcPhonPict
@ 08,04 SAY '........ SOLD TO ........'
@ 08,46 SAY '........... SHIP TO ...........'
@ 09,04 SAY Account
@ 09,11 SAY Customer.Phone1 PICTURE lcPhonPict
*:E301517,4 MHM to get store [Start]
*@ 09,46 SAY "STORE : " + Store + "  DC : " + Customer.Dist_Ctr
@ 09,46 SAY "STORE : " + IIF(lcMulTi = 'Y',&lcTempOrd..Store,OrdHdr.Store)+ "  DC : " + Customer.Dist_Ctr
*:E301517,4 MHM [end]
*:E301517,4 MHM [Start]
*@ 10,04 SAY lcSolTName
@ 10,04 SAY lcBtName
*:E301517,4 MHM [End]
@ 10,46 SAY lcShpTName
@ 11,04 SAY laSoldTo[1]
@ 11,46 SAY laShipTo[1]
@ 12,04 SAY laSoldTo[2]
@ 12,46 SAY laShipTo[2]
@ 13,04 SAY laSoldTo[3]
@ 13,46 SAY laShipTo[3]

*C126257,1 NNA 02/12/2005 (Begin) Display laShipTo[4]
@ 14,46 SAY laShipTo[4]
*C126257,1 NNA (End)

@ 15,00 SAY REPLICATE('*',80)
@ 16,00 SAY 'SHIPVIA       | SEASON       | SPCL INSTRUCTIONS | STORE#  | DEPT# | CUST P.O. |'
@ 17,00 SAY REPLICATE('*',80)
*:E301517,4 MHM  02/01/2001 [End]
*@ 18,00 SAY lcShipVia
*@ 18,14 SAY '| '+ lcSeason
*@ 18,29 SAY '| '+ lcSpcInst
*@ 18,49 SAY '| '+ Store
@ 18,00 SAY LEFT(lcShipVia,13)
@ 18,14 SAY '| '+ LEFT(lcSeason,13)
@ 18,29 SAY '| '+ LEFT(lcSpcInst,18)
@ 18,49 SAY '| '+  IIF(lcMulTi = 'Y',&lcTempOrd..Store,OrdHdr.Store)
*:E301517,4 MHM  02/01/2001 [End]
@ 18,59 SAY '| '+ OrdHdr.Dept
*:E301517,4 MHM  02/01/2001 [Start]
*@ 18,67 SAY '| '+ IIF(OrdHdr.MultiPO,&lcTempOrd..CustPo,OrdHdr.CustPo)+'|'
@ 18,67 SAY '| '+ IIF(OrdHdr.MultiPO,LEFT(&lcTempOrd..CustPo,10),LEFT(OrdHdr.CustPo,10))+'|'
*:E301517,4 MHM  02/01/2001 [End]
@ 19,00 SAY REPLICATE('*',80)
@ 20,00 SAY '      GRP ' + lcMajTitle + SPACE(19-LEN(lcMajTitle+lcNonMajTl)) + lcNonMajTl + ;
            '                        PRICE  PIECES       AMOUNT'
@ 21,00 SAY REPLICATE('*',80)
*-- End of lpPrintHdr.
*!***************************************************************************
*! Name      : lfGetOdHdr
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 03/01/2001
*! Purpose   : Function to get the information of the ORDER header.
*!***************************************************************************
*! Called from : None.
*!***************************************************************************
*! Calls       : None.
*!***************************************************************************
*! Passed Parameters : None.
*!***************************************************************************
*! Return            : None
*!***************************************************************************
*! Example           : = lfGetOdHdr()
*!***************************************************************************
*
FUNCTION lfGetOdHdr
PRIVATE lcAlias , lcCurrKey
lcAlias = ALIAS()
lcCurrKey = ""

lcOrder = &lcTempOrd..Order
lcMulti   = OrdHdr.Multi
lcAccount = Ordhdr.Account
SELECT (lcTempOrd)
lcStore = IIF(lcMulti <> 'Y',ORDHDR.STORE,lcStore)  
STORE '' TO lcBtAddr1,lcBtAddr2,lcBtAddr3,lcBtAddr4,lcBtAddr5,lcBtAddr6

*-- GET DESCRIPTIONS FOR CODED FIELDS
SELECT CODES
*--Terms
lcTermData =SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
*--ShipVia
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Division long name.
STORE '' TO lcDivLName
=gfRltFld(OrdHdr.cDivision,@laDivLName,'CDIVISION')
*--Special instruction
lcSpcInst = SUBSTR(gfCodDes(OrdHdr.SPCINST , 'SPCINST'),1,15)
*---Season
lcSeason = SUBSTR(gfCodDes(OrdHdr.SEASON , 'SEASON'),1,10)
*--Get the first line of the company address.

*B607321,1 KHM 06/02/2003 (Begin) Getting the term's description.
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
*B607321,1 KHM 06/02/2003 (End)


IF lcMulti = "Y" 
  SELECT CUSTOMER
  =SEEK('S'+ &lcTempOrd..Account+ &lcTempOrd..Store)

  *B804441,1 MHM 10/25/2001 Get ShipVia at store level [Start]
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  
  *B804441,1 MHM [End]

  lcBtName  = CUSTOMER.BTNAME
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
  
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    
    *C126257,1 NNA 02/12/2005 (Begin) Display OrdHdr.cAddress5 in a Separate Line
    *laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    laShipTo[3] = ALLTRIM(OrdHdr.cAddress3)+IIF(!EMPTY(OrdHdr.cAddress3),',','')+;
                  ALLTRIM(OrdHdr.cAddress4)
    laShipTo[4] = OrdHdr.cAddress5
    *C126257,1 NNA (End)
    
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    * Print distribution center address if found 
    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcStore = lcStore + '  Dist. Center : ' + Customer.Store
    ENDIF

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
    laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ENDIF 
ELSE
  lcBtName  = CUSTOMER.BTNAME
  SELECT CUSTOMER
  
  *B607321,1 KHM 06/02/2003 (Begin) Getting the right sold to information
  *= gfGetAdr('CUSTOMER','','','',@laSoldTo,'')
  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')  
  *B607321,1 KHM 06/02/2003 (End)
  
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

  *-- SHIP_TO ADDRESS FOR THIS STORE
  STORE '' TO lcStName,lcStAddr1,lcStAddr2,lcStAddr3,lcStAddr4,lcStAddr5,lcStAddr6
  IF OrdHdr.Alt_ShpTo
    lcShptName  = OrdHdr.STNAME
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2

    *C126257,1 NNA 02/12/2005 (Begin) Display OrdHdr.cAddress5 in a Separate Line
    *laShipTo[3] = OrdHdr.cAddress3+','+OrdHdr.cAddress4+','+OrdHdr.cAddress5
    laShipTo[3] = ALLTRIM(OrdHdr.cAddress3)+IIF(!EMPTY(OrdHdr.cAddress3),',','')+;
                  ALLTRIM(OrdHdr.cAddress4)
    laShipTo[4] = OrdHdr.cAddress5
    *C126257,1 NNA (End)
    
    IF Len(ALLTRIM(lcStAddr2)) = 0
      lcStAddr2 = lcStAddr3 
    ENDIF
  ELSE
    * Print distribution center address if found 
    IF !EMPTY(CUSTOMER.DIST_CTR)  
      lcCurrKey = 'S' + Customer.Account + Customer.Store
      =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
      lcStore = lcStore + '  Dist. Center : ' + Customer.Store
    ENDIF

    lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
    laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
    laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
    laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

    IF !EMPTY(lcCurrKey)
      = SEEK(lcCurrKey , 'CUSTOMER')
    ENDIF
  ENDIF 

ENDIF
= lfAdrShift('laShipTo')  && Shift Sold To address if there is empty line.
laCompAdd1 = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
laCompAdd1 = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1

NOTELOOP = .F.

SELECT (lcAlias)

*-- End OF lfGetOdHdr.