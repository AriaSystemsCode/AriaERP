*:***************************************************************************
*: Program file  : ALPKTKLS.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM FOR LAWRENCE STEVENS.
*: Date          : 06/30/2003
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Khalid Mohi El-din
*: Tracking Job Number: 200577
*:***************************************************************************
*: Example : DO ALPKTKLS
*:***************************************************************************
*: Modifications
*: B119039,1 ABD 10/15/2003 Let the user to Select the Printer driver name, and
*: B119039,1 ABD            Accept Zero copies and din't print any copeis.
*:***************************************************************************

STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst
=lfEvalSegs()


llWareLoc = (ALLTRIM(UPPER(gfGetMemVar('M_WARELOC'))) = 'Y')
IF llWareLoc
  SET ORDER TO WhsLocSt IN WhsLoc
ENDIF
SET ORDER TO TAG SpckLins IN SPCK_LIN

STORE 0 TO lnTotPik, lnTotPrice, lnRow
DIMENSION laScales[4], laAddress[1,1]
STORE '' TO laAddress, laScales
*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
STORE .F. TO llPrint
STORE 1 TO ln1stCopy, ln2ndCopy
STORE '' TO lc1stPort, lc2ndPort

*B119039,1 ABD - Define 2 new fields to hold the printer name. [Begin]
STORE '' TO lc1PrntNam , lc2PrntNam
*B119039,1 ABD - [End]


SELECT (lcTmpOrdL)
SET FILTER TO cGrupDetal = "D"
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  llNoRec = .T.
  RETURN
ENDIF

IF VARREAD() = "PBRUN"
   SET DEVICE TO SCREEN
   DO (gcScrDir+'AL\ALSELPRT.SPX')
   IF llPrint

     *B119039,1 ABD - IF Condation to didin't allow to the user to print  Zero Copies. [Begin]
      IF ln1stCopy > 0
        *B119039,1 ABD - [End]
     
        SET DEVICE TO PRINTER
        lcOldPort = gcprnt_port
        gcprnt_port = lc1stPort
        llOnce = .T.
        gcOutFile = gcWorkDir+gfTempName()+'.TXT'
        lcTmpCurs = gfTempName()
        CREATE CURSOR (lcTmpCurs) (cCopyPage M )
        APPEND BLANK
        = lfPrnPkTk(ln1stCopy)
        
        *B119039,1 ABD - Send Teh Printer Drive for every port. [Begin]
        *= LFPRNTJOB()
        = lfPrntJob(lc1PrntNam)
        *B119039,1 ABD - [End]
        
        gcprnt_port = lcOldPort

        *B119039,1 ABD - End if for if condation. [Begin]
      ENDIF
      *B119039,1 ABD - [End]
      
      IF ln2ndCopy > 0
        SET DEVICE TO PRINTER
        lcOldPort = gcprnt_port
        gcprnt_port = lc1stPort
        llOnce = .T.
        gcOutFile = gcWorkDir+gfTempName()+'.TXT'
        lcTmpCurs = gfTempName()
        CREATE CURSOR (lcTmpCurs) (cCopyPage M )
        APPEND BLANK
        = lfPrnPkTk(ln2ndCopy)


        *B119039,1 ABD - Send Teh Printer Drive for every port. [Begin]
        *= LFPRNTJOB()
        = lfPrntJob(lc2PrntNam)
        *B119039,1 ABD - [End]
        
        
        gcprnt_port = lcOldPort
      ENDIF

      SET DEVICE TO SCREEN
      llNoRec = .T.
   ELSE
   
    SET DEVICE TO SCREEN
    llNoRec = .T.
    RETURN
  ENDIF
ELSE
  =lfPrnLines()
  llNoRec = .F.
ENDIF


*!**************************************************************************
*! Name      : lfPrnPkTk
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : When print to printer
*!**************************************************************************
*! Example   :  =lfPrnPkTk()
*!**************************************************************************
FUNCTION lfPrnPkTk
PARAMETER lnTotCopes
SET DEVICE TO PRINTER
STORE 0 TO lnTotPik, lnTotPrice
SELECT (lcTmpOrdL)
LOCATE
DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  
  lcPickTkt  = PikTkt
  lcOrder    = Order
  lcStore    = Store
  lcAccount  = OrdHdr.Account
  lcCustPO   = IIF(OrdHdr.MultiPO,&lcTmpOrdL..CustPo,OrdHdr.CustPO)
  lcDept     = OrdHdr.Dept
  ldStartDt  = OrdHdr.Start
  ldComplDt  = OrdHdr.Complete
  lcShipVia  = OrdHdr.ShipVia
  lcRep1     = OrdHdr.Rep1
  lcApproval = OrdHdr.Approval

  SET PRINTER TO
  LCTMPTXT = gcWorkDir+GFTEMPNAME()+'.TXT'
  SET PRINTER TO (LCTMPTXT)

  =lfPrintHdr()
  =lfGetScale()
  =lfPrntScal()
  STORE 0 TO lnTotPik, lnTotPrice
  
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6) = lcPickTkt+lcOrder+'D'
    IF lnRow > 54
      =lfPrnContI()
      = lfGenTxtFl('A')
      =lfPrintHdr()
      =lfPrntScal()
    ENDIF

    @ lnRow, 1 SAY  SUBSTR(STYLE,1,lnMajorLen)
    @ lnRow, 13 SAY SUBSTR(Style,lnNonMajSt,lnColorLen)
    @ lnRow, 20 SAY SUBSTR(ALLTRIM(STYLE.DESC),1,10)
    @ lnRow, 31 SAY ALLTRIM(SCALE)
    @ lnRow, 32 SAY PIK1 PICTURE '@Z 9999'
    @ lnRow, 37 SAY PIK2 PICTURE '@Z 9999'
    @ lnRow, 42 SAY PIK3 PICTURE '@Z 9999'
    @ lnRow, 47 SAY PIK4 PICTURE '@Z 9999'
    @ lnRow, 52 SAY PIK5 PICTURE '@Z 9999'
    @ lnRow, 57 SAY PIK6 PICTURE '@Z 999'
    @ lnRow, 60 SAY PIK7 PICTURE '@Z 999'
    @ lnRow, 63 SAY PIK8 PICTURE '@Z 999'
    @ lnRow, 67 SAY TOTPIK PICTURE '@Z 99999'
    
    *-- If print the price
    IF llRpStyPrc
      @ lnRow, 72 SAY PRICE PICTURE '99999.99'
    ENDIF

    *-- If print line notepad
    IF llRpOrdLnt .AND. !EMPTY(OrdLine.NOTE_MEM)
      = lfPrnLnNot()
    ENDIF
    
    *-- If print style bins
    IF llRpStyLoc .AND. llWareLoc .AND. SEEK(Style,'WhsLoc')
      = lfPrnLoc()
    ENDIF

    *-- If print sku 
    IF llRpSkuBck .AND. SEEK('S'+lcAccount+Style,'Spck_Lin')
      = lfPrntSku()
    ENDIF

    lnRow = lnRow + 2
    lnTotPik = lnTotPik + TotPik
    lnTotPrice = lnTotPrice + (TotPik * Price)
    
  ENDSCAN

  IF llRpOrdNot .AND. SEEK ('B' + lcOrder,'NotePad')
    =lfPrnNotes()
  ENDIF
    
  =lfPrOptMsg()
  =lfPrntTot()
  =lfGenTxtFl('B')

  IF SEEK(lcPickTkt,"PIKTKT")
    SELECT PIKTKT
    REPLACE PIKTKT.PrtFlag WITH 'P'
  ENDIF  
  SELECT (lcTmpOrdL)
ENDDO
WAIT CLEAR

*!**************************************************************************
*! Name      : lfPrnLines
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print pick tikcet lines
*!**************************************************************************
*! Example   :  =lfPrnLines()
*!**************************************************************************
FUNCTION lfPrnLines

SET DEVICE TO PRINTER
STORE 0 TO lnTotPik, lnTotPrice
SELECT (lcTmpOrdL)
LOCATE
DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  
  lcPickTkt  = PikTkt
  lcOrder    = Order
  lcStore    = Store
  lcAccount  = OrdHdr.Account
  lcCustPO   = IIF(OrdHdr.MultiPO,&lcTmpOrdL..CustPo,OrdHdr.CustPO)
  lcDept     = OrdHdr.Dept
  ldStartDt  = OrdHdr.Start
  ldComplDt  = OrdHdr.Complete
  lcShipVia  = OrdHdr.ShipVia
  lcRep1     = OrdHdr.Rep1
  lcApproval = OrdHdr.Approval
 
  =lfPrintHdr()
  =lfGetScale()
  =lfPrntScal()
  STORE 0 TO lnTotPik, lnTotPrice
  
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6) = lcPickTkt+lcOrder+'D'
    IF lnRow > 54
      =lfPrnContI()
      =lfPrintHdr()
      =lfPrntScal()
    ENDIF

    @ lnRow, 1 SAY  SUBSTR(STYLE,1,lnMajorLen)
    @ lnRow, 13 SAY SUBSTR(Style,lnNonMajSt,lnColorLen)
    @ lnRow, 20 SAY SUBSTR(ALLTRIM(STYLE.DESC),1,10)
    @ lnRow, 31 SAY ALLTRIM(SCALE)
    @ lnRow, 32 SAY PIK1 PICTURE '@Z 9999'
    @ lnRow, 37 SAY PIK2 PICTURE '@Z 9999'
    @ lnRow, 42 SAY PIK3 PICTURE '@Z 9999'
    @ lnRow, 47 SAY PIK4 PICTURE '@Z 9999'
    @ lnRow, 52 SAY PIK5 PICTURE '@Z 9999'
    @ lnRow, 57 SAY PIK6 PICTURE '@Z 999'
    @ lnRow, 60 SAY PIK7 PICTURE '@Z 999'
    @ lnRow, 63 SAY PIK8 PICTURE '@Z 999'
    @ lnRow, 67 SAY TOTPIK PICTURE '@Z 99999'
    
    *-- If print the price
    IF llRpStyPrc
      @ lnRow, 72 SAY PRICE PICTURE '99999.99'
    ENDIF

    *-- If print line notepad
    IF llRpOrdLnt .AND. !EMPTY(OrdLine.NOTE_MEM)
      = lfPrnLnNot()
    ENDIF
    
    *-- If print style bins
    IF llRpStyLoc .AND. llWareLoc .AND. SEEK(Style,'WhsLoc')
      = lfPrnLoc()
    ENDIF

    *-- If print sku 
    IF llRpSkuBck .AND. SEEK('S'+lcAccount+Style,'Spck_Lin')
      = lfPrntSku()
    ENDIF

    lnRow = lnRow + 2
    lnTotPik = lnTotPik + TotPik
    lnTotPrice = lnTotPrice + (TotPik * Price)
    
  ENDSCAN

  IF llRpOrdNot .AND. SEEK ('B' + lcOrder,'NotePad')
    =lfPrnNotes()
  ENDIF
    
  =lfPrOptMsg()
  =lfPrntTot()
ENDDO
WAIT CLEAR

*!**************************************************************************
*! Name      : lfPrintHdr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the header of the picking ticket.
*!**************************************************************************
*! Example   :  =lfPrintHdr()
*!**************************************************************************
FUNCTION lfPrintHdr
PRIVATE lnAlias

lnAlias = SELECT(0)
=lfCustInfo()
*ash1 move the whole P/T one line up
@ 2, 26  SAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
@ 3, 2   SAY gdSysDate
@ 3, 26  SAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
@ 4, 26  SAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
@ 5, 26  SAY 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
@ 05,62  SAY lcOrder
@ 05,72  SAY lcPickTkt
@ 09, 5  SAY lcBtName
@ 09, 51 SAY lcStName
@ 10, 5  SAY lcBtAdd1
@ 10, 51 SAY lcStAdd1
@ 11, 5  SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd2,lcBtAdd3)
@ 11, 51 SAY IIF(!EMPTY(lcStAdd2),lcStAdd2,lcStAdd3)
@ 12, 5  SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd3,'')
@ 12, 51 SAY IIF(!EMPTY(lcStAdd2),lcStAdd3,' ')
@ 16, 01 SAY lcAccount
@ 16, 08 SAY ALLTRIM(SUBSTR(lcStore,1,7))
@ 16, 15 SAY ALLTRIM(lcCustPO)
@ 16, 26 SAY ALLTRIM(lcDept)
@ 16, 32 SAY ldStartDt
@ 16, 42 SAY ldComplDt
@ 16, 51 SAY ALLTRIM(gfCodDes(lcShipVia,'SHIPVIA  '))
@ 16, 66 SAY ALLTRIM(lcRep1)
@ 16, 70 SAY ALLTRIM(lcApproval)

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the scale
*!**************************************************************************
*! Example   :  =lfGetScale()
*!**************************************************************************
FUNCTION lfGetScale
PRIVATE lnAlias, lnOrdLRecN

lnAlias = SELECT(0)

DIMENSION laScales[4]
laScales   = ' '
llFrstTime = .T.
lnCounter  = 1
lnOrdLRecN = RECNO('OrdLine')
=SEEK('O'+lcOrder,'OrdLine')
SELECT OrdLine
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) =  'O'+lcOrder ;
          FOR Store = lcStore AND PikTkt = lcPickTkt
   IF llFrstTime
      laScales[1] = Scale
      llFrstTime = .F.
   ELSE
     IF lnCounter < 4
       IF ASCAN(laScales,Scale) = 0
         lnCounter = lnCounter + 1
         laScales[lnCounter] = Scale
       ENDIF
     ENDIF
   ENDIF
ENDSCAN
IF BETWEEN(lnOrdLRecN,1,RECCOUNT('OrdLine'))
  GOTO lnOrdLRecN IN OrdLine
ENDIF 
SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrntScal
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the scales
*!**************************************************************************
*! Example   :  =lfPrntScal()
*!**************************************************************************
FUNCTION lfPrntScal
PRIVATE lnAlias

lnAlias = SELECT(0)

*ash1
*lnSRow = 22
lnSRow = 21
FOR lnCount = 1 TO 4
   ln1st4Size = 32
   ln2nd4Size = 57
   IF !EMPTY(laScales(lnCount)) .AND. SEEK('S'+laScales(lnCount),'Scale')
      @ lnSRow, 31 SAY ALLTRIM(laScales(lnCount))
      *ash1
      *lnSCol = 32
      lnSCol = 31
      FOR lnScalCnt = 1 TO 8
         lcScalCnt = STR(lnScalCnt,1)
         IF lnScalCnt > 5
            @ lnSRow,ln2nd4Size SAY PADL(SUBSTR(ALLTRIM(Scale.Sz&lcScalCnt),1,3),3)
            ln2nd4Size = ln2nd4Size + 4
         ELSE
            @ lnSRow,ln1st4Size SAY PADL(ALLTRIM(Scale.Sz&lcScalCnt),4)
            ln1St4Size = ln1St4Size + 5
         ENDIF
      ENDFOR
      lnSRow = lnSRow + 1
   ENDIF
ENDFOR
*ash1
*lnRow = 25
lnRow = 24

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrnLnNot
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the line notepad.
*!**************************************************************************
*! Example   :  =lfPrnLnNot()
*!**************************************************************************
FUNCTION lfPrnLnNot
PRIVATE lnAlias

lnAlias = SELECT(0)

lnRow = lnRow + 1
lnOldMemW = SET('MEMOWIDTH')
lnNotLine = 1
SET MEMOWIDTH TO 65
lnMemLins = MEMLINES(OrdLine.NOTE_MEM)

DO WHILE lnNotLine <= lnMemLins
   IF lnRow >= 55
     =lfPrnContI()
     IF VARREAD() = "PBRUN"
       =lfGenTxtFl('A')
     ENDIF  
     
     =lfPrintHdr()
     =lfPrntScal()
   ENDIF
   @ lnRow, 13 SAY MLINE(OrdLine.NOTE_MEM,lnNotLine)
   lnRow = lnRow + 1
   lnNotLine = lnNotLine + 1
ENDDO
lnRow = lnRow - 1
SET MEMOWIDTH TO lnOldMemW

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrnLoc
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the style bins
*!**************************************************************************
*! Example   :  =lfPrnLoc()
*!**************************************************************************
FUNCTION lfPrnLoc

PRIVATE lnAlias

lnAlias = SELECT(0)
lnRow = lnRow + 1
IF lnRow >= 55
  =lfPrnContI()
  IF VARREAD() = "PBRUN"
    =lfGenTxtFl('A')
  ENDIF  
  =lfPrintHdr()
  =lfPrntScal()
ENDIF

@ lnRow, 13 SAY 'Locations : '
lnColWd = 25

SELECT WhsLoc
                
SCAN REST WHILE Style+Color+cWareCode+cLocation = &lcTmpOrdL..Style
  IF lnRow >= 55
    =lfPrnContI()
    IF VARREAD() = "PBRUN"
      =lfGenTxtFl('A')
    ENDIF
    =lfPrintHdr()
    =lfPrntScal()
   ENDIF

   IF lnColWd < 70
      @ lnRow,lnColWd SAY ALLTRIM(WhsLoc.cLocation)
      lnColWd = lnColWd + LEN(ALLTRIM(WhsLoc.cLocation)) + 2
   ELSE
      lnRow = lnRow + 1
      IF lnRow >= 55
        =lfPrnContI()
        IF VARREAD() = "PBRUN"
          =lfGenTxtFl('A')
        ENDIF
        =lfPrintHdr()
        =lfPrntScal()
      ENDIF
      lnColWd = 13
      @ lnRow, lnColWd SAY ALLTRIM(WhsLoc.cLocation)
      lnColWd = lnColWd + LEN(ALLTRIM(WhsLoc.cLocation)) + 2
   ENDIF
ENDSCAN

SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfPrntSku
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the style Skus
*!**************************************************************************
*! Example   :  =lfPrntSku()
*!**************************************************************************
FUNCTION lfPrntSku
PRIVATE lnAlias

lnAlias = SELECT(0)

lnCustRec = RECNO('Customer')
= SEEK('M'+lcAccount,'Customer')
lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')

IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
   lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
   lnDime2 = SkuTmpl.Len4
ELSE
   lnDime1 = 8
   lnDime2 = 8
ENDIF

IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
   GOTO IN CUSTOMER lnCustRec
ENDIF

lnRow = lnRow + 1
IF lnRow >= 55
  =lfPrnContI()
  IF VARREAD() = "PBRUN"
    =lfGenTxtFl('A')
  ENDIF
  =lfPrintHdr()
  =lfPrntScal()
ENDIF

lcSkuHdr = 'SKU# : '+IIF(UPPER(lcSkuTmpl) $ 'JCP*MER' ,;
            LEFT(Spck_Lin.Pack_Id,SkuTmpl.Len1)+' '+SUBSTR(Spck_Lin.Pack_Id,SkuTmpl.Len1+1,SKUTMPL.LEN2),;
            LEFT(Spck_Lin.Pack_Id,lnDime1))
@ LNROW, 13 SAY lcSkuHdr
IF !llRpSkuSiz
  SELECT(lnAlias)
  RETURN
ENDIF

DIMENSION laSku(8,2)
laSku = ''
lnCount = 0
lnColWd = 13+LEN(ALLTRIM(lcSkuHdr)) + 1

SELECT SPCK_LIN
SCAN REST WHILE Type+Account+Style+Pack_Id = 'S'+lcAccount+&lcTmpOrdL..STYLE
   lnCount = lnCount + 1
   FOR lnQtyN = 1 TO 8
      IF EVALUATE('Spck_Lin.Qty'+ALLTRIM(STR(lnQtyN))) = 1
         laSku(lnQtyN, 1) = STR(lnQtyN)
         laSku(lnQtyN, 2) = SUBSTR(PACK_ID,lnDime1+1,lnDime2)
         EXIT
      ENDIF
   ENDFOR
ENDSCAN
=ASORT(laSku,1)
FOR lnQtyN = 1 TO lnCount
   IF EVALUATE('OrdLine.Pik'+ALLTRIM(laSku(lnQtyN,1))) <> 0
      IF lnRow >= 55
        =lfPrnContI()
        IF VARREAD() = "PBRUN"
          =lfGenTxtFl('A')
        ENDIF         
        =lfPrintHdr()
        =lfPrntScal()
      ENDIF

      IF lnColWd < 70
         @ lnRow, lnColWd SAY 'S'+ALLTRIM(laSku(lnQtyN,1))+':'+ALLTRIM(laSku(lnQtyN,2))
         lnColWd = lnColWd + LEN(ALLTRIM(laSku(lnQtyN,2))) + 4
      ELSE
         lnRow = lnRow + 1
         IF lnRow >= 55
           =lfPrnContI()
           IF VARREAD() = "PBRUN"
             =lfGenTxtFl('A')
           ENDIF
           =lfPrintHdr()
           =lfPrntScal()
         ENDIF
         lnColWd = 13
         @ lnRow, lnColWd SAY 'S'+ALLTRIM(laSku(lnQtyN,1))+':'+ALLTRIM(laSku(lnQtyN,2))
         lnColWd = lnColWd + LEN(ALLTRIM(laSku(lnQtyN,2))) + 4
      ENDIF
   ENDIF
ENDFOR
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfPrnNotes
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the notepad
*!**************************************************************************
*! Example   :  =lfPrnNotes()
*!**************************************************************************
FUNCTION lfPrnNotes

PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine
lnAlias = SELECT()
lnOldMemW = SET('MEMOWIDTH')
lnNotLine = 1
SET MEMOWIDTH TO 65
lnRow = lnRow + 1

SELECT NOTEPAD
lnMemLins = MEMLINES(mNotes)
DO WHILE lnNotLine <= lnMemLins
   IF lnRow >= 55
     =lfPrnContI()
     IF VARREAD() = "PBRUN"
       =lfGenTxtFl('A')
     ENDIF
     =lfPrintHdr()
     =lfPrntScal()
   ENDIF
   IF !EMPTY(MLINE(mNotes,lnNotLine))
      @ lnRow, 4 SAY MLINE(mNotes,lnNotLine)
      lnRow = lnRow + 1
   ENDIF
   lnNotLine = lnNotLine + 1
ENDDO
SET MEMOWIDTH TO lnOldMemW
SELECT (lnAlias)

*!**************************************************************************
*! Name      : lfPrOptMsg
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print optional messages.
*!**************************************************************************
*! Example   :  =lfPrOptMsg()
*!**************************************************************************
PROCEDURE lfPrOptMsg

FOR lnLinCnt = 1 TO 3
   lcLinCnt = STR(lnLinCnt,1)
   IF !EMPTY(lcRpMsg&lcLinCnt)
     lnRow = lnRow + 1
     IF lnRow >= 55
       =lfPrnContI()
       IF VARREAD() = "PBRUN"
         =lfGenTxtFl('A')
       ENDIF
       =lfPrintHdr()
       =lfPrntScal()
      ENDIF
      @ lnRow,13 SAY LEFT(ALLTRIM(lcRpMsg&lcLinCnt),70)
   ENDIF
ENDFOR

*!**************************************************************************
*! Name      : lfPrntTot
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the totals
*!**************************************************************************
*! Example   :  =lfPrntTot()
*!**************************************************************************
PROCEDURE lfPrntTot
*ash1
@ 54, 67 SAY lnTotPik PICTURE '99999'
@ 54, 72 SAY lnTotPrice PICTURE '99999.99'
IF VARREAD() = "PBRUN"
   @ 0, 0 SAY ''
ENDIF

*!**************************************************************************
*! Name      : lfPrnContI
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the continue word.
*!**************************************************************************
*! Example   :  =lfPrnContI()
*!**************************************************************************
FUNCTION lfPrnContI
*ash1
@ 56, 3 SAY '* * * C O N T I N U E D * * *'
@ 58, 69 SAY '****'
@ 58, 74 SAY '*******'

IF VARREAD() = "PBRUN"
   @ 0, 0 SAY ''
ENDIF

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Khalid Mohi El-Din Mohamed
*: Date        : 06/30/2003
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
*PARAMETER lcReturn
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*!**************************************************************************
*! Name      : lfCustInfo
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To get the customer addresses.
*!**************************************************************************
*! Example   :  =lfCustInfo()
*!**************************************************************************
FUNCTION lfCustInfo
PRIVATE lcAlias,lnCount

lcAlias = SELECT(0)
DIMENSION laAddress[1,1]
STORE '' TO laAddress
*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6

lcCusExp   = IIF(EMPTY(lcStore),"M","S") + lcAccount +IIF(EMPTY(lcStore),"",lcStore)
=SEEK(lcCusExp,'Customer')

*-- Bill to Address
lcBtName  = CUSTOMER.BtName
=gfGetAdr('Customer','','','',1,'2')

*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = ALLTRIM(lcBtAdd&lcCount) + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR

*-- Sold to Address  
IF OrdHdr.Alt_ShpTo
  lcStName = OrdHdr.STNAME
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = OrdHdr.cAddress3
  lcStAdd4 = OrdHdr.cAddress4
  lcStAdd5 = OrdHdr.cAddress5
ELSE
  lcStName = Customer.STNAME
  =gfGetAdr('Customer','','','',1,'')
  *--Get the Ship To adddess.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = ALLTRIM(lcStAdd&lcCount) + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  	
ENDIF  
SELECT (lcAlias)

*!**************************************************************************
*! Name      : lfvOK
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : The valid function for OK button
*!**************************************************************************
*! Example   :  =lfvOK()
*!**************************************************************************
FUNCTION lfvOK

lc1stPort = Pu1stPort
lc2ndPort = Pu2ndPort
llPrint = .T.

*!**************************************************************************
*! Name      : lfGenTxtFl
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the number of jobs
*!**************************************************************************
*! Example   :  =lfGenTxtFl()
*!**************************************************************************
FUNCTION lfGenTxtFl
PARAMETER LCTYPE

SET PRINTER TO
APPEND MEMO &lcTmpCurs..cCopyPage FROM &lcTmpTxt OVERWRITE
IF LLONCE
  SET PRINTER TO &gcOutFile
  @ 0, 0 SAY ''
  SET PRINTER TO
  LLONCE = .F.
ENDIF
SET PRINTER TO &gcOutFile 
FOR LNNOOFCOPY = 1 TO LNTOTCOPES
  COPY MEMO &lcTmpCurs..cCopyPage TO &gcOutFile  ADDITIVE    
ENDFOR
SET PRINTER TO
ERASE (LCTMPTXT)
IF LCTYPE='A'
  LCTMPTXT = gcWorkDir+GFTEMPNAME()+'.TXT'
  SET PRINTER TO (LCTMPTXT)
ENDIF


*!**************************************************************************
*! Name      : lfPrntJob
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 06/30/2003
*! Purpose   : To print the job
*!**************************************************************************
*! Example   :  =lfPrntJob()
*!**************************************************************************
FUNCTION lfPrntJob

*B119039,1 ABD - Send Teh Printer Drive for every port. [Begin]
PARAMETER lcSndParms
IF TYPE('lcSndParms') = "U"
  RETURN
ENDIF
lcOldsetup = _PDSETUP
_PDSETUP   = lcSndParms
*B119039,1 ABD - [End]

lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))
lcPrntTool = IIF(EMPTY(lcPrntTool) AND _WINDOWS,'TYPE',lcPrntTool)
lcPrntTool = IIF(_WINDOWS,GETENV('ARIAPRN'),IIF(GETENV('ARIAPRN')='LIB',"",GETENV('ARIAPRN')))


lcPdDriv = _PDRIVER
lcPDSetup = _PDSETUP
gcLoclPrt = gcPrnt_Port 
SET PRINTER TO
SET PRINTER TO (gcPrnt_Port)


llError=.F.
lcOnErr=ON('ERROR')
ON ERROR llError=.T.
lcSetSafe=SET('SAFE')
SET SAFE OFF
DO WHILE SYS(13)='OFFLINE'
  IF gfModalGen("QRM00054B00015",'ALERT') = 2
    llError=.T.
    EXIT
  ENDIF
ENDDO 
 
IF FILE(gcOutFile) AND llError=.F.
  =lfSetEscSeq()
  DO CASE
    CASE lcPrntTool = '???' OR lcPrntTool = '??' OR lcPrntTool = '?'
      lcTmpCur = gfTempName() && temprory cursor
      lcWorkA = SELECT()      && curr. work area
      CREATE CURS &lcTmpCur (mprnfile M) && create temp. cursor
      APPEND BLANK 
      *-- append memo with the output file
      APPEND MEMO mprnfile FROM (gcOutFile) 
      IF glPrnt_Lan                && if it is a lan option
        lcPrintSet = '\\SPOOLER\P='+ALLTRIM(STR(gnPrnt_No))+'\S='+ALLTRIM(gcServ_Nam);
                    +'\Q='+ALLTRIM(gcQueu_nam);
                    +IIF(_pCopies>1,"\C="+ALLTRIM(STR(_PCOPIES)),"");
                    +IIF(glBaner,'\B='+gcBaner_H,'\N')
        SET PRINTER TO &lcPrintSet        
      ELSE
        SET PRINTER TO (gcLoclPrt)
      ENDIF       
      SET PRINTER ON
      lnMemoWid = SET('MEMOW')
      lcConsSet = SET('CONS')
      llWrapStat = _WRAP
      lnIndent = _INDENT
      lnRMargin = _RMARGIN
      lnLMargin = _LMARGIN
      SET CONS OFF
      SET MEMOW TO 65              
      _WRAP = .T.
      _RMARGIN = 255
      _LMARGIN = 0
      _INDENT = 2
      *-- reset the printing cursor then start printing the memo field
      FOR lnNoCopy = 1 TO _PCOPIES
        IF lcPrntTool == '???'
          &lcPrntTool ALLTRM(mprnfile)
        ELSE
          &lcPrntTool MPRNFILE FUNCTION 'V253'
        ENDIF  
      ENDFOR
      SET PRINTER OFF
      SET PRINTER TO
      SET CONS &lcConsSet
      SET MEMOW TO lnMemoWid
      _WRAP = llWrapStat
      _INDENT = lnIndent 
      _RMARGIN = lnRMargin 
      _LMARGIN = lnLMargin 
              
      *-- close the temp. cursor and select the curr. work area
      USE IN &lcTmpCur
      SELECT (lcWorkA)          

    CASE lcPrntTool="COPY"
      lcHeadSet = SET('HEADING')
      lnMemoWid = SET('MEMOW')
      lcConsSet = SET('CONS')
      llWrapStat = _WRAP
      lnRMargin = _RMARGIN
      lnLMargin = _LMARGIN
      lcAdvance = _PADVANCE
      SET MEMOW TO 255              
      _WRAP = .F.
      _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))      
      _LMARGIN = 0
      _PADVANCE = 'LINEFEED'
      SET HEADING OFF
      SET CONS OFF
      SET PRINTER TO (gcLoclPRT)  
        
      FOR lnNoCopy = 1 TO _PCOPIES
        !COPY &gcOutFile. &gcLoclPRT
      ENDFOR  

      SET HEADING &lcHeadSet
      SET CONS &lcConsSet
      SET MEMOW TO lnMemoWid
      _WRAP = llWrapStat
      _RMARGIN = lnRMargin 
      _LMARGIN = lnLMargin 
      _PADVANCE = lcAdvance

    CASE lcPrntTool="LIB"
      IF FILE("TXTSPOOL.FLL") AND FILE("PRTWIN.EXE") 
        SET LIBRARY TO TXTSPOOL.FLL ADDITIVE
        lcWinPrt  = "IBM Proprinter on " + ALLTRIM(gcLoclPRT)
        lcOrgPrnt = PRTWIN(lcWinPrt)
        FOR lnNoCopy = 1 TO _PCOPIES
          =TXTSPOOL(gcOutFile,"ARIA SYSTEMS")
        ENDFOR  
        = PRTWIN(lcOrgPrnt)
        RELEASE LIBRARY TXTSPOOL.FLL
      ENDIF
    CASE lcPrntTool = 'TYPE'
      lcHeadSet = SET('HEADING')
      lnMemoWid = SET('MEMOW')
      lcConsSet = SET('CONS')
      llWrapStat = _WRAP
      lnRMargin = _RMARGIN
      lnLMargin = _LMARGIN
      lcAdvance = _PADVANCE
      SET MEMOW TO 255              
      _WRAP = .F.
      _RMARGIN  = IIF(R_Width='XW',240,IIF(R_Width='W',132,80))      
      _LMARGIN = 0
      _PADVANCE = 'LINEFEED'
      SET HEADING OFF
      SET CONS OFF
      lcTempPr=_PDSETUP
      SET PRINTER TO (gcPrnt_Port)
      FOR lnNoCopy = 1 TO _PCOPIES
        TYPE (gcOutFile) TO PRINTER
      ENDFOR  
      SET PRINTER TO 
      SET HEADING &lcHeadSet
      SET CONS &lcConsSet
      SET MEMOW TO lnMemoWid
      _PDSETUP = lcTempPr
      _WRAP = llWrapStat
      _RMARGIN = lnRMargin 
      _LMARGIN = lnLMargin 
      _PADVANCE = lcAdvance
    OTHERWISE
      FOR lnNoCopy = 1 TO _PCOPIES
        COPY FILE (gcOutFile) TO (gcPrnt_Port)
      ENDFOR  
      *B802847,1 Hesham (End)
  ENDCASE

	  SET PRINTER OFF
  _PDSETUP = lcPDSetup
  
ENDIF
ON ERROR &lcOnErr
SET SAFE &lcSetSafe    

*B119039,1 ABD - Return to old Port. [Begin]
_PDSETUP = lcOldsetup
*B119039,1 ABD - [End]

ERASE (gcOutFile)

*B119039,1 ABD - [Begin]
*!*************************************************************
*! Name      : lfvSetup    
*! Developer : Abdou ElGendi
*! Date      : 10/15/2003
*! Purpose   : Valid Function For Printer Driver Check Box
*! Reference : B119039,1
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*!
FUNCTION lfvSetup    
PARAMETER lcSendParm

IF TYPE('lcSendParm') = "U"
  RETURN
ENDIF

lcResouce = SET('RESOU')
SET RESOU ON

IF _WINDOWS
  IF TYPE('OGPlatForm')='C' AND (OGPlatForm='DOS' OR OGPlatForm='UNIX')
    lcTPrint = _PDSETUP
    _PDSETUP = "?"
    IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
      rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
      lcServ_Nam = gcServ_Nam
      lcQueu_Nam = gcQueu_Nam
    ENDIF
    lcPrnt_Port = gcPrnt_Port
    lcWinPrint = IIF(!EMPTY(_PDSETUP),_pdsetup,'None')
    lc&lcSendParm.PrntNam = lcWinPrint
    SHOW GETS
  ELSE
    =SYS(1037)
  ENDIF
ELSE
  lcTPrint = _PDSETUP
  _PDSETUP = "?"
  IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
    rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
    lcServ_Nam = gcServ_Nam
    lcQueu_Nam = gcQueu_Nam
  ENDIF
  SHOW GETS
ENDIF  
SET RESOU &lcResouce

*-- End OF lfvSetup.
*B119039,1 ABD - [End]
***************************************************************************************