*:**************************************************************************
*! Name : ALPKTKDO.PRG (CONVERTED FROM 26 TO 27 FOR DORBY )
*! Auth : Adel Mohhamed El Gazzar (ADEL)
*: DESC.: PRINT INVOICE - 7 7/16 * 8 7/16"  (For DORBY)
*! Date : 06/23/1999
*! Refer to : (C101552)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfPriHdr()
*:                   : lfLoadVar()
*:					 : lfGetSiz()
*:                   : lfPrnNotes()
*:                   : lfPrntSku()
*:                   : lfPrnLnNot()
*:**************************************************************************
*: Modifications :
*:*B802707,1 BWA 10/14/1999 Fix the bug of "End of file encountered" If we 
*:*B802707,1 BWA            print any pick ticket and order notepad = "NO" .
*:*B802711,1 RENEE 11/09/99 Set page length to 51
*:*B802823,1 ABD 12/01/1999 Fix bug THAT FOR MULTI-STOER MULTI-PO#, THE PT DO NOT
*:*B802823,1 ABD            PRINT PO FOR ALL PTs.
*:B803013,1 SAM 01/30/2000  Fix Bug of print zip code out of report 
*:C101892,1 RAMY 06/11/2000 Print the BOL# in tthe form
*:B803766,1 AAN 11/02/2000 Fix bug that not print correct Bill to address
*:C102352,1 AMM 05/16/2001 Add printing of Serial #
*:**************************************************************************

*B802711,1 Set page length to 51
_pLength = 51
*B802711,1 end
*---Open nessisary file
llNoRec     = .F.    
= gfOpenFile(gcDataDir+'SkuTmpl',gcDataDir+'SkuTmpl','SH')
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
*C102352,1 AMM Open ASN_SHIP file and get the UCC manufactorer ID
= gfOpenFile(gcDataDir+'ASN_SHIP',gcDataDir+'ASN_SHIP','SH')
lcMFID = PADL(ALLTRIM(GFGETMEMVAR('XMANUFID',gcAct_Comp)),7,'0')
lcSerNo = SPACE(0)
*C102352,1 AMM end

*C101892,1 RAMY [start]
= gfOpenFile(gcDataDir+'Pack_Hdr',"" ,'SH')
SET ORDER TO Orderpck
SELECT PIKTKT
SET RELATION TO ORDER+STORE+PIKTKT INTO Pack_Hdr ADDITIVE
*C101892,1 RAMY [end]

*--Restore the global flags.
llMultiWH  = (gfGetMemVar('M_WareHouse') = 'Y')
lcDuns     = gfGetMemVar('xDUNS')
*-- Get the major and nonmajor titles and lengths.
lcMajTitle = ALLTRIM(gfItemMask('HM'))
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
DIMENSION laAddress[1]
DIMENSION laSku(8,2)
*--Initiliaze the needed variables.
STORE ' ' TO lcInvoice,lcShipVia,lcTermData,lcNotes,lcInvDate,lcWar1,lcWar2,lcWar3,laAddress
STORE ' ' TO lcSiz,lcOldSiz,laSku,lcQty,lcPikTkt,lcStyle,lcColor,lcSlsMn,lcOrder
STORE ' ' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcWar1,lcWar2,lcWar3,lcBtAdd4,lcBtAdd5,lcBtAddr
STORE ' ' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
STORE ' ' TO lcAccount,lcStore
STORE 0   TO lnQtyNo,lnCount,lnColWd,lnMemLins,lnNotLine,lnOldMemW,lnQtyn,lnPageNo
STORE 0   TO lnHdrRow,lnLinRow,MaxRow,lnBotRow,lnPrXRow

*C101892,1 RAMY [start]
STORE ' ' TO lcBOL
*C101892,1 RAMY [end]

lcInvDate = SPACE(01)
lcSiz 	  = SPACE(40)
lnInvNo   = 22
lcVDisc   = SPACE(7)
lcTDisc   = SPACE(7)
*-------------
*-- MAIN LOOP
*-------------
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
*-- This file holds all the lines of the selected pick tickets.
SELECT (lcTmpOrdL)
GOTO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  llNoRec = .T.
  RETURN
ENDIF

DO WHILE !EOF() AND INKEY() <>32
  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  *-- Function to get piktkt HEADER information.  
  =lfLoadVar()
  *--Print the piktkt header information. 
  =lfPriHdr()
  *--Print the report body.
  
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    SCATTER MEMVAR
    WAIT WINDOW ORDER+'/'+SUBSTR(m.Style,1,lnMajorLen)+'/'+SUBSTR(m.Style,lnNonMajSt,LEN(lcNonMajPi)) NOWAIT
    lcStyle = SUBSTR(Style,1,lnMajorLen)
    lcColor = SUBSTR(Style,lnNonMajSt,LEN(lcNonMajPi))
    lcStyCol = STYLE
    lcOrder = Order
    =lfGetSiz()
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
      =lfPriHdr()
    ENDIF
    IF lcOldSiz <> lcSiz
      lcOldSiz = lcSiz
      @ lnLinRow,18 SAY lcSiz
      lnLinRow = lnLinRow + 1
    ENDIF
    @ lnLinRow,01 SAY ALLT(SUBSTR(lcStyle,1,9))
    @ lnLinRow,09 SAY TotPik  PICTURE '99999'
    lcIndex = ''
    lnPos   = 17
    FOR lnIndex = 1 TO lnQtyNo
      lcIndex = ALLTRIM(STR(lnIndex))
      @ lnLinRow,lnPos SAY Pik&lcIndex PICTURE '99999'
      lnPos = lnPos + 5
    ENDFOR
    IF llRpStyPrc
      @ lnLinRow,56 SAY Price PICTURE '999999.99'
      @ lnLinRow,65 SAY Price*TotPik PICTURE '9999999.99'
    ENDIF
    lnLinRow = lnLinRow + 1
    IF lnLinRow > MaxRow
      =lfPriHdr()
    ENDIF
    @ lnLinRow,17 SAY lcColor
    @ lnLinRow,25 SAY Style.Desc
    IF llRpOrdLNt
      =lfPrnLnNot()
    ENDIF
    IF llRpSkuSiz AND SEEK('S'+lcAccount+lcStyCol,'Spck_Lin')
      =lfPrntSku()
    ENDIF
  ENDSCAN
  
  IF !EMPTY(lcRpMsg1)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
       =lfPriHdr()
    ENDIF
    @ lnLinRow,02 SAY lcRpMsg1
  ENDIF
  IF !EMPTY(lcRpMsg2)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
       =lfPriHdr()
    ENDIF
    @ lnLinRow,02 SAY lcRpMsg2
  ENDIF
  IF !EMPTY(lcRpMsg3)
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
       =lfPriHdr()
    ENDIF
    @ lnLinRow,02 SAY lcRpMsg3
  ENDIF
  
  IF llRpOrdNot AND SEEK('B'+lcOrder,'NotePad')
    =lfPrnNotes()
  ENDIF
  @ lnPrXRow,33 SAY 'XXXXXXXXXXX'
  WAIT CLEAR

  SELECT (lcTmpOrdL)
  
  *B802707,1 BWA 10/14/1999 Fix the bug of "End of file encountered" [START]
  *SKIP
  IF llRpOrdNot
    SKIP
  ENDIF
  *B802707,1 BWA 10/14/1999 [END]

ENDDO
SET DEVICE TO SCREEN
RETURN

*--- End Program.

*:*************************************************************************
*: PROGRAM   : lfPriHdr 
*: DESC.     : PRINT INVOICE Header
*: MODULE    : Aria Apparel Series.
*: DATE      : 06/24/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr

lnPageNo = lnPageNo + 1
IF lnInvNo = 22
  lnHdrRow = 03
  lnLinRow = 28
  lnBotRow = 47
  lnPrXRow = 50
  MaxRow   = lnBotRow
  lnInvNo  = 1
ELSE
  lnHdrRow = lnBotRow + 07
  lnLinRow = lnHdrRow + 26
  lnBotRow = lnLinRow + 18
  lnPrXRow = lnLinRow + 21
  MaxRow   = lnBotRow
  lnInvNo  = lnInvNo  + 01
ENDIF
lcOldSiz = ''
*---Start print From
DECLARE laDivLName[1,2]
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
@ lnHdrRow,05 SAY IIF(!EMPTY(lcDivLName),lcDivLName,lcCompName)
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY laCompAdd[1]
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY laCompAdd[2]
lnHdrRow = lnHdrRow + 1 
@ lnHdrRow,05 SAY laCompAdd[3]
lnHdrRow = lnHdrRow + 2
DO CASE
  CASE OrdHdr.cDivision = 'MD    '
    lcDuns = '09-208-0480'
  CASE OrdHdr.cDivision = 'LD    '
    lcDuns = '09-208-0480'
  CASE OrdHdr.cDivision = 'DF    '    
    lcDuns = '00-150-9538'
ENDCASE
@ lnHdrRow,10 SAY lcDuns
*---End print From.
lnHdrRow = lnHdrRow + 3

*B803013,1 [Start] Shift printing position to left
*@ lnHdrRow,57 SAY lcWar1
*--Ramy
*@ lnHdrRow,53 SAY lcWar1
@ lnHdrRow,57 SAY lcWar1
*--Ramy
*B803013,1 [End]

lnHdrRow = lnHdrRow + 1

*B803013,1 [Start] Shift printing position to left
*@ lnHdrRow,57 SAY lcWar2
*--Ramy
*@ lnHdrRow,53 SAY lcWar2
@ lnHdrRow,57 SAY lcWar2
*--Ramy
*B803013,1 [End]

lnHdrRow = lnHdrRow + 1

*B803013,1 [Start] Shift printing position to left
*@ lnHdrRow,57 SAY lcWar3
*--Ramy
*@ lnHdrRow,53 SAY lcWar3
@ lnHdrRow,57 SAY lcWar3
*--Ramy
*B803013,1 [End]
lnHdrRow = lnHdrRow + 3
@ lnHdrRow,05 SAY lcBtName
@ lnHdrRow,34 SAY SPACE(10)
@ lnHdrRow,44 SAY lcStName
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY lcBtAdd1
@ lnHdrRow,44 SAY lcStAdd1
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY lcBtAdd2
@ lnHdrRow,44 SAY lcStAdd2
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,05 SAY lcBtAdd3
@ lnHdrRow,44 SAY lcStAdd3   
lnHdrRow = lnHdrRow + 2
@ lnHdrRow,10 SAY lcAccount
lnHdrRow = lnHdrRow + 1
@ lnHdrRow,10 SAY lcStore
@ lnHdrRow,21 SAY SPACE(01)
@ lnHdrRow,30 SAY OrdHdr.Dept
@ lnHdrRow,36 SAY SPACE(9)
@ lnHdrRow,44 SAY lcShipVia

*B802823,1  Get Cust PO from ORDLINE if the order is multi PO. [ Begin ]
*@ lnHdrRow,63 SAY OrdHdr.CustPo
IF OrdHdr.MultiPo
  @ lnHdrRow,63 SAY Ordline.CustPo
ELSE
  @ lnHdrRow,63 SAY OrdHdr.CustPo
ENDIF
*B802823,1 [ End ]

lnHdrRow = lnHdrRow + 3
@ lnHdrRow,01 SAY lcInvDate
@ lnHdrRow,13 SAY lcInvoice
@ lnHdrRow,23 SAY lcTermData
@ lnHdrRow,42 SAY lcOrder
@ lnHdrRow,51 SAY lcPikTkt
@ lnHdrRow,61 SAY lcSlsMn
lnHdrRow = lnHdrRow + 1

*C101892,1 RAMY [start]
*IF llRpOrdNot
*  @ lnHdrRow,10 SAY lcNotes
*ENDIF
@ lnHdrRow,10 SAY 'BOL #' + SPACE(2) + lcBOL
*C101892,1 RAMY [end]

*C102352,1 AMM Print the serial Number 
@lnHdrRow,30 SAY 'Serial # '+lcSerNo 
*C102352,1 AMM end

*:*************************************************************************
*: PROGRAM   : lfLoadVar (C DORBY)
*: DESC.     : PRINT INVOICE Header
*: MODULE    : Aria Apparel Series.
*: DATE      : 06/24/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfLoadVar

lcInvoice  = ''
*-Take the store ship via if the user cleared the ship via in case of a multi-store order.
lcShipVia = SUBSTR(gfCodDes(OrdHdr.SHIPVIA , 'SHIPVIA'),1,15)
*--Terms
lcTermData = SUBSTR(gfCodDes(OrdHdr.CTERMCODE , 'CTERMCODE'),1,15)
lcNotes    = OrdHdr.Note1+OrdHdr.Note2
lcInvDate  = SPACE(01)
lcPikTkt   = PikTkt
lcSlsMn    = OrdHdr.Rep1
lcAccount  = PikTkt.Account
lcStore    = PikTkt.Store
lcOrder    = Order

*C101892,1 RAMY [start]
lcBOL      = Pack_Hdr.Bill_Ladg
*C101892,1 RAMY [end]
*C102352,1 AMM Get the serial Number value
IF SEEK(Pack_Hdr.Bill_Ladg+PikTkt.PikTkt,'ASN_SHIP')
  lcSerNo = "(00) 0"+ASN_SHIP.Manuf_id+ASN_SHIP.UCC9+ASN_SHIP.UCC_check
ELSE
  lcSerNo = lfGetSer()
ENDIF
*C102352,1 AMM end

STORE SPACE(30) TO lcWar1,lcWar2,lcWar3
IF llMultiWH

  *C101892,1 RAMY [start]
  *lcWar1 = WAREHOUS.cAddress1
  *lcWar2 = WAREHOUS.cAddress2
  *lcWar3 = SUBSTR(WAREHOUS.cAddress3,1,15)+' ' +SUBSTR(WAREHOUS.cAddress4,1,3)+' '+ SUBSTR(WAREHOUS.cAddress5,1,10)

  lcWar1 = ALLTRIM(WAREHOUS.cAddress1)
  lcWar2 = ALLTRIM(WAREHOUS.cAddress2)
  lcWar3 = ALLTRIM(SUBSTR(WAREHOUS.cAddress3,1,15))+' ' +;
           ALLTRIM(SUBSTR(WAREHOUS.cAddress4,1,3))+' '+;
           ALLTRIM(SUBSTR(WAREHOUS.cAddress5,1,10))
  *C101892,1 RAMY [end]
  
  IF EMPTY(lcWar2)
    lcWar2 = lcWar3
    lcWar3 = ''
  ENDIF
ENDIF

*-- SHIP_TO ADDRESS FOR THIS STORE
=IIF(EMPTY(lcStore),SEEK('M'+lcAccount,'Customer'),SEEK('S'+lcAccount+lcStore,'Customer'))
STORE ' ' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6
IF OrdHdr.Alt_ShpTo
  lcStName = ALLT(OrdHdr.STName)+IIF(!EMPTY(OrdHdr.Store),' STORE #'+OrdHdr.Store,'')
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = SUBSTR(OrdHdr.cAddress3,1,15)+ ',' +;
             SUBSTR(OrdHdr.cAddress4,1,3)+ ',' + SUBSTR(OrdHdr.cAddress5,1,10)
ELSE
  IF !EMPTY(CUSTOMER.Dist_Ctr)
    =SEEK("S"+CUSTOMER.Account+CUSTOMER.Dist_Ctr,"CUSTOMER")
  ENDIF
  lcStName  = IIF(EMPTY(Customer.DBA),ALLT(Customer.STNAME),ALLT(Customer.DBA))+;
              IIF(!EMPTY(OrdHdr.Store),' STORE #'+OrdHdr.Store,'')
  =gfGetAdr('CUSTOMER','','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
  ENDFOR  
ENDIF  

*-- Bill TO ADDRESS
*:B803766 AAN Fix bug that not print correct Bill to address[Start]
=SEEK(IIF(EMPTY(lcStore),"M","S") + lcAccount + lcStore,"CUSTOMER")
*:B803766 AAN Fix bug that not print correct Bill to address[End]

STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAddr

lcBtName  = CUSTOMER.BTNAME
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR

****************************************************************************
* PROG : lfGetSiz.PRG   (C# DORBY)
* DESC : FUNCTION TO GET STYLE SIZES.
* DATE : 06/24/1999
* AUTH : Adel Mohhamed El Gazzar (ADEL)
****************************************************************************
FUNCTION lfGetSiz

lcSiz = ''
IF !EOF('SCALE')
  lnQtyNo = SCALE.Cnt
  FOR lnIndex = 1 TO SCALE.Cnt
    lcIndex = ALLTRIM(STR(lnIndex))
    lcSiz   = lcSiz+PADL(ALLTRIM(SCALE.SZ&lcIndex),4)+ IIF(lnIndex<>SCALE.Cnt,' ','')
  ENDFOR
ELSE
  lcSiz = SPACE(40)
ENDIF

*!*************************************************************
*! Name      : lfPrnLnNot
*: DATE      : 06/24/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the order line notes.
*!*************************************************************
*! Example            :  lfPrnLnNot()
*!*************************************************************
FUNCTION lfPrnLnNot

lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 70
lnMemLins = MEMLINES(OrdLine.Note_Mem)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(OrdLine.Note_Mem,lnNotLine)) 
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
       =lfPriHdr()
    ENDIF
    @ lnLinRow,02 SAY MLINE(OrdLine.Note_Mem,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO     
SET MEMOWIDTH TO lnOldMemW

*!*************************************************************
*! Name      : lfPrntSku
*: DATE      : 06/24/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the style's Skus.
*!*************************************************************
*! Example   :  lfPrntSku()
*!*************************************************************
FUNCTION lfPrntSku
PRIVATE lnAlias

lnAlias = SELECT(0)
*-- Getting the SKU templet
lnCustRec = RECNO('Customer')
=SEEK('M'+OrdLine.Account,'Customer')
lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8
  lnDime2 = 8
ENDIF 
IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
  GOTO lnCustRec IN Customer
ENDIF

*-- Print the Main Sku of the style
lcSkuHdr = "SKU# : "+LEFT(Spck_Lin.Pack_Id,SkuTmpl.Len1)+' '+;
               SUBSTR(Spck_Lin.Pack_Id,SkuTmpl.Len1+1,SkuTmpl.Len2)
lnLinRow = lnLinRow + 1
IF lnLinRow >MaxRow
   =lfPriHdr()
ENDIF
@ lnLinRow,06 SAY ALLTRIM(lcSkuHdr)
*-- If the user did not select to print the sku by sizes.
IF !llRpSkuSiz
  RETURN
ENDIF
 
DIMENSION laSku(8,2)
laSku   = ''
lnCount = 0   
lnColWd = 2+LEN(ALLTRIM(lcSkuHdr))+1
SELECT Spck_Lin
SCAN REST WHILE Type+Account+Style = ;
                'S' + lcAccount+lcStyCol
  lnCount = lnCount + 1
  IF lnCount>8
    lnCount = 8
    EXIT
  ENDIF
  FOR lnQtyn = 1 TO 8
    IF EVAL('Spck_Lin.Qty'+ALLTRIM(STR(lnQtyn))) = 1
      laSku(lnCount, 1) = STR(lnQtyn)          
      laSku(lnCount, 2) = SUBSTR(Pack_Id,lnDime1+1,lnDime2)
      EXIT
    ENDIF
  ENDFOR
ENDSCAN
=ASORT(laSku,1)
FOR lnQtyn = 1 TO lnCount
  lcQty = ALLTRIM(laSku(lnQtyn,1))
  IF !EMPTY(lcQty)
    IF OrdLine.Pik&lcQty <> 0
      IF lnLinRow >MaxRow
       =lfPriHdr()
      ENDIF
      IF lnColWd < 70
          @ lnLinRow,lnColWd SAY "S"+ALLTRIM(laSku[lnQtyn,1])+":"+;
          ALLTRIM(laSku[lnQtyn,2])                 
         lnColWd = lnColWd + LEN(ALLTRIM(laSku[lnQtyn,2])) + 4
      ELSE
       lnLinRow = lnLinRow + 1
       IF lnLinRow >MaxRow
        =lfPriHdr()
       ENDIF
       lnColWd = 2+LEN(ALLTRIM(lcSkuHdr))+1
       @ lnLinRow,lnColWd SAY "S"+ALLTRIM(laSku[lnQtyn,1])+":"+;
       ALLTRIM(laSku[lnQtyn,2])
       lnColWd = lnColWd + LEN(ALLTRIM(laSku[lnQtyn,2])) + 4
      ENDIF
    ENDIF
  ENDIF
ENDFOR
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfPrnNotes
*: DATE      : 06/24/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the order's notepad.
*!*************************************************************
*! Example   :  lfPrnNotes()
*!*************************************************************
FUNCTION lfPrnNotes
PRIVATE lnAlias

lnOldMemW = 0
lnMemLins = 0
lnNotLine = 0
lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 65
SELECT NotePad
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(MNOTES,lnNotLine))
    lnLinRow = lnLinRow + 1
    IF lnLinRow >MaxRow
     =lfPriHdr()
    ENDIF
    @ lnLinRow,02 SAY MLINE(MNOTES,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO     
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)



*!*************************************************************
*! Name      : lfGetSer
*: DATE      : 15/05/2001
*: Developer : Ahmed Mohamed Ibrahim
*! Purpose   : To get the serial Number
*! Ref       : *C102352,1 AMM
*!*************************************************************
*! Example   :  lfGetSer()
*!*************************************************************

FUNCTION lfGetSer
PRIVATE lcGetSer
lcGetSer = lcMFID+SUBSTR(PIKTKT,2)+'0001'+LFGETCHCKD('000'+lcMFID+SUBSTR(PIKTKT,2)+'0001')
lcGetSer = "(00) 0"+lcGetSer

RETURN lcGetSer


*!*************************************************************
*! Name      : LFGETCHCKD
*: DATE      : 15/05/2001
*: Developer : Ahmed Mohamed Ibrahim
*! Purpose   : To get the check digit
*! Ref       : *C102352,1 AMM
*!*************************************************************
*! Example   :  LFGETCHCKD()
*!*************************************************************
FUNCTION LFGETCHCKD
PARAMETERS lcUccNo
PRIVATE lnChkDigit,lnSumOdd,lnSumEven,lnCount

STORE 0 TO lnSumOdd,lnSumEven,lnChkDigit
FOR lnCount = 1 TO 9
  lnSumOdd  = lnSumOdd + VAL(SUBSTR(lcUccNo,lnCount*2-1,1))
  lnSumEven = lnSumEven+ VAL(SUBSTR(lcUccNo,lnCount*2,1))
ENDFOR
lnSumOdd   = lnSumOdd + VAL(SUBSTR(lcUccNo,19,1))
lnChkDigit = MOD(lnSumOdd*3+lnSumEven,10)
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))

