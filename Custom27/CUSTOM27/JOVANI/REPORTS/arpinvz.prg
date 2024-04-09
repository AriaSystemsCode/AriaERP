****************************************************************************
*: Program file  : ARPINVRZ.PRG (C# 101482)
*: Program desc. : PRINT INVOICE  ON DOT MATRIX PRINTER, 80 COL ,64 LINE.
*:               : (Cust.: JOVANI)
*:               : Convert INV810z from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:**************************************************************************
*: Calls : FUNCTIONS  : lfSlctRcrd,lfAdrShift
*:       : PROCEDURES : lpPrntHdr,lpPrntDetl,lpPrntFotr
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*:  Modifications:
*:B802896,1 MHM 06/26/2000 Print Style location.
****************************************************************************
llAlocation = ('AL' $ gcCmpModules)
llNoRec   = .F.
llPckUsed = .F.
IF llAlocation
  llPckUsed = gfOpenFile(gcDataDir+'Pack_Hdr' ,'Pack_Hdr','SH')
ENDIF
llOrdUsed = gfOpenFile(gcDataDir+'OrdLine' ,'OrdLine','SH')
llLocUesd = gfOpenFile(gcDataDir+'whsloc' ,'Whslocst','SH')
INVHTEMP  = gfTempName()
SELECT INVHDR
COPY ALL FOR &lcRpExp TO &gcWorkDir.&INVHTEMP
= gfOpenFile('&gcWorkDir.&INVHTEMP',' ','EX')
SELECT (INVHTEMP)
GO TOP
IF EOF()
   =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  RETURN
ENDIF
InvLTemp = gfTempName()
llPrntNote = (gfModalGen('QRM40156B40000','DIALOG' ) = 1)
WAIT WINDOW 'Selecting records ... ' NOWAIT
=lfSlctRcrd()
CLEAR TYPEAHEAD
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
lnTotQty  = 0
PageNo    = 0
llNewPage = .T.
MaxRow    = 64
LineLimit = 54
ROW       = 99
lnTotAmnt = 0
lnTaxes   = 0
lnFreight = 0
lcAlias   = ''

*---Main Loop
SELECT (INVHTEMP)
GO TOP
DO WHILE INKEY() <> 32 AND !EOF()
  lcCurrInv = Invoice
  IF llNewPage
    SELECT InvHdr 
    =RLOCK()
    REPLACE PrtFlag WITH 'P'
    UNLOCK
    SELECT (InvHTemp)
    DO lpPrntHdr
    llNewPage =.F.
  ENDIF
  IF ROW > MaxRow
    llNewPage = .T.
    LOOP
  ENDIF
  SELECT (INVLTEMP)
  SCAN REST WHILE Invoice+Style = lcCurrInv AND INKEY()<> 32
    IF ROW > LineLimit-2
      @ ROW+1, 20 SAY '********** C O N T I N U E D ***********'
      llNewPage =.T.
      EXIT
    ENDIF
    DO lpPrntDetl
  ENDSCAN
  IF (lcCurrInv <> Invoice)
    DO lpPrntFotr
    SELECT (INVHTEMP)
    SKIP
  ENDIF
  SELECT (InvHTemp)
ENDDO
USE IN IIF(llPckUsed,'Pack_Hdr',0)
USE IN IIF(llPckUsed,'OrdLine',0)
USE IN IIF(llLocUesd,'whsloc',0)
IF USED(INVHTEMP)
  USE
  ERASE (INVHTEMP).DBF
  ERASE (INVHTEMP).FPT
ENDIF
IF USED(InvLTemp)
  USE
  ERASE (InvLTemp).DBF
  ERASE (InvLTemp).FPT
  ERASE (InvLTemp).CDX
ENDIF

RETURN
*!*************************************************************
*! Name      : lfSlctRcrd.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/15/1998
*! Purpose   : Open all the needed files for the invoice.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSlctRcrd()
*!************************************************************
FUNCTION lfSlctRcrd

SELECT InvLine
COPY STRUCTURE TO (gcWorkDir+InvLTemp)
=gfOpenFile(gcWorkDir+InvLTemp,'','EX')
SELECT (InvLTemp)
INDEX ON INVOICE+STYLE TAG (InvLTemp)
SELECT (INVHTEMP)
SCAN
  IF SEEK(&INVHTEMP..INVOICE,'INVHDR')
    lcAlias=IIF(INVHDR.CONSOL = 'Y','CONSINVL','INVLINE')
    IF SEEK(&InvHTemp..Invoice,lcAlias)
      SELECT (lcAlias)
      SCAN WHILE INVOICE = &INVHTEMP..INVOICE
        SCATTER MEMVAR MEMO
        INSERT INTO (InvLTemp) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN
SELECT InvHdr
SET RELATION TO Invoice INTO ConsInvH ADDITIVE
SELECT (InvHTemp)
SET RELATION TO Invoice INTO InvHdr, Invoice INTO (InvLTemp) ADDITIVE
GOTO TOP
RETURN

*!*************************************************************
*! Name      : lpPrntHdr .
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/15/1998
*! Purpose   : Open all the needed files for the invoice.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : DO lpPrntHdr 
*!************************************************************
PROCEDURE lpPrntHdr 

SELECT InvHdr
lnTaxes   = Tax_Amt
lnFreight = Freight
@ 2, 64 SAY Invoice
@ 5, 62 SAY Account
@ 6, 52 SAY 'ORDER NO: '+ORDER
IF llAlocation     
  @ 7, 52 SAY 'PACK LIST NO: '
  IF SEEK(PikTkt,'Pack_Hdr')
    @ 7, 66 SAY PikTkt
  ENDIF
ENDIF  
lcAccount = Account
lcStore   = Store
lcOrder   = Order
SELECT CUSTOMER
STORE '' TO  lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5
DECLARE laAddress[6,3]
=SEEK (IIF(EMPTY(lcStore),'M'+lcAccount,'S'+lcAccount+lcStore))
STORE Customer.BtName TO lcSolTName
=gfGetAdr('Customer','','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBillAdd&lcCount = lcBillAdd&lcCount + IIF(EMPTY(lcBillAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR  
SELECT ORDHDR
=SEEK('O'+lcOrder)
IF ORDHDR.Alt_ShpTo
  lcShpTName = ORDHDR.STName
  lcShipAdd1 = ORDHDR.cAddress1
  lcShipAdd2 = ORDHDR.cAddress2
  lcShipAdd3 = ORDHDR.cAddress3
  lcShipAdd4 = ORDHDR.cAddress4
  lcShipAdd5 = ORDHDR.cAddress5
ELSE
  IF INVHDR.CONSOL = 'Y'
    SELECT CUSTOMER
    =SEEK('S'+lcAccount+CONSINVH.STORE)
  ENDIF
  SELECT CUSTOMER
  lcShpTName  = IIF( EMPTY(DBA) , CUSTOMER.STName , DBA)
  STORE IIF(EMPTY(Customer.Dba),Customer.StName,Customer.Dba) TO lcShpTName
  =gfGetAdr('CUSTOMER','','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcShipAdd&lcCount = lcShipAdd&lcCount + IIF(EMPTY(lcShipAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ENDIF
@ 13,07 SAY ALLTRIM(lcSolTName)
@ 13,53 SAY ALLTRIM(lcShpTName)
@ 14,07 SAY ALLTRIM(lcBillAdd1)
@ 14,53 SAY ALLTRIM(lcShipAdd1)
@ 15,07 SAY ALLTRIM(lcBillAdd2)
@ 15,53 SAY ALLTRIM(lcShipAdd2)
@ 16,07 SAY ALLTRIM(lcBillAdd3)
@ 16,53 SAY ALLTRIM(lcShipAdd3)
@ 17,07 SAY ALLTRIM(lcBillAdd4)+ALLTRIM(lcBillAdd5)
@ 17,53 SAY ALLTRIM(lcShipAdd4)+ALLTRIM(lcShipAdd5)
SELECT InvHdr
@ 23,01 SAY InvDate
lcShiVCode = gfCodDes(InvHdr.ShipVia,'SHIPVIA   ')
@ 23, 10 SAY SUBSTR(lcShiVCode,1,7)
IF SEEK('O'+ORDER,'OrdHdr')
  SELECT OrdHdr
  @ 23,18 SAY Entered
ENDIF
SELECT InvHdr
@ 23, 27 SAY SUBSTR(CustPo,1,8)
@ 23, 37 SAY Rep1
lcTerVCode = gfCodDes(cTermCode,'CTERMCODE ')
@ 23,48 SAY SUBSTR(lcTerVCode,1,15)
@ 23,67 SAY IIF(Consol='Y',' ',STORE)
@ 24,37 SAY Rep2
@ 25,01 SAY 'ORD  SHP'
@ 25,60 SAY 'Dept :' + Dept
ROW = 28
RETURN

*!*************************************************************
*! Name      : lpPrntDetl .
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/15/1998
*! Purpose   : Prints the Invoice Lines.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : DO lpPrntDetl
*!************************************************************
PROCEDURE lpPrntDetl

IF SEEK(&InvLTemp..Order+STR(&InvLTemp..LineNo,6),'OrdLine')
  @ ROW, 1 SAY (OrdLine.TotQty+ &InvLTemp..TotQty) PICTURE '999'
ENDIF
SELECT (InvLTemp)
@ ROW, 6 SAY TotQty PICTURE '999'
SELECT (InvLTemp)
@ ROW, 10 SAY STYLE
@ ROW, 32 SAY Scale
lnCnt = 0
lnMax = 0
IF SEEK('S'+Scale,'Scale')
  SELECT Scale
  @ ROW, 34 SAY cScl_Desc
  *---cordtype+order+STR(lineno,6)
  lcPin = IIF(SEEK('O'+&InvLTemp..ORDER+STR(&InvLTemp..lineno,6),'OrdLine');
               AND SEEK(&InvLTemp..Style+SPACE(06)+OrdLine.cWareCode,'WhsLoc'),WhsLoc.cLocation,'')
  @ ROW, 47 SAY SUBSTR(lcPin,1,5)
  *:B802896,1 MHM 06/26/2000 Print Style location[Begin]
  SELECT Style
  =SEEK(&InvLTemp..Style)
  @ ROW, 60 SAY 'Location :' +Style.Location
  SELECT Scale
  *:B802896,1 MHM 06/26/2000 [End]

  ROW = ROW + 1
  lnMax = CNT
  FOR lnCnt = 1 TO lnMax
    Z = STR(lnCnt,1)
    @ ROW, 10+((lnCnt-1)*6) SAY PADL(ALLTRIM(SZ&Z),5)
  ENDFOR
ELSE
  *---cordtype+order+STR(lineno,6)
  lcPin = IIF(SEEK('O'+&InvLTemp..ORDER+STR(&InvLTemp..lineno,6),'OrdLine');
               AND SEEK(&InvLTemp..Style+SPACE(06)+OrdLine.cWareCode,'WhsLoc'),WhsLoc.cLocation,'')
  @ ROW, 47 SAY SUBSTR(lcPin,1,5)
  *:B802896,1 MHM 06/26/2000 Print Style location[Begin]
  SELECT Style
  =SEEK(&InvLTemp..Style)
  @ ROW, 60 SAY 'Location :' +Style.Location
  SELECT (InvLTemp)
  *:B802896,1 MHM 06/26/2000 [End]

  ROW = ROW + 1
  lnMax = 8
ENDIF
SELECT (InvLTemp)
FOR lnCnt = 1 TO lnMax
  Z = STR(lnCnt,1)
  @ ROW+1,10+((lnCnt-1)*6) SAY Qty&Z PICTURE '99999'
ENDFOR
@ ROW+1, 60 SAY Price
@ ROW+1, 70 SAY (TotQty * Price) PICTURE '9999999.99'
lnTotAmnt = lnTotAmnt + (TotQty * Price)
lnTotQty  = lnTotQty+TotQty
ROW =ROW + 2
RETURN

*!*************************************************************
*! Name      : lpPrntFotr .
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 03/15/1998
*! Purpose   : Prints the Invoice Footer lines.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : DO lpPrntFotr
*!************************************************************
PROCEDURE lpPrntFotr

IF llPrntNote
  SELECT InvHdr
  @ ROW,   1 SAY Note1 + SPACE(1) + Note2
  ROW = ROW + 2
ENDIF
SELECT InvHdr
@ 55, 02 SAY 'TotQty       '
@ 55, 16 SAY lnTotQty      PICTURE '999999.99'
@ 56, 02 SAY 'NO OF CARTONS'
@ 56, 16 SAY Cartons       PICTURE '999999.99'
@ 57, 02 SAY 'WEIGHT'
@ 57, 16 SAY Weight        PICTURE '999999.99'
SELECT InvHdr
llPrtDisc = (Discount <> 0)
IF llPrtDisc
  @ 57, 45 SAY 'NONTAXABLE SUBTOTAL'
  @ 57, 65 SAY lnTotAmnt PICTURE '99999999.99'
ENDIF
@ 58,02 SAY 'DATE SHIP'
@ 58,16 SAY '  /  /'
IF llPrtDisc
  @ 58, 45 SAY 'MERCHANDISE DESC.  '
  @ 58, 65 SAY Discount PICTURE '99999999.99'
ELSE
  @ 58, 45 SAY 'NONTAXABLE SUBTOTAL'
  @ 58, 65 SAY lnTotAmnt PICTURE '99999999.99'
ENDIF
@ 59, 45 SAY 'TAX'
@ 59, 65 SAY lnTaxes PICTURE '99999999.99'
@ 60, 45 SAY 'TAXABLE SUBTOTAL'
@ 60, 65 SAY (lnTotAmnt+lnTaxes+Discount) PICTURE '99999999.99'
@ 61, 45 SAY 'FREIGHT'
@ 61, 65 SAY lnFreight PICTURE '99999999.99'
IF COD <> 0
  @ 62, 45 SAY "C.O.D. CHARGE"
  @ 62, 65 SAY COD PICTURE '99999999.99'
  llTotRow = 63
ELSE  
  llTotRow = 62
ENDIF
@ llTotRow, 45 SAY 'TOTAL'
@ llTotRow, 65 SAY (lnTotAmnt+Discount+lnTaxes+lnFreight+COD) ;
                   PICTURE '99999999.99'
ROW       = 99
lnTotAmnt = 0
lnTotQty  = 0
lnTaxes   = 0
lnFreight = 0
RETURN
