*****************************************************************************
*: Program file  : SOORCNSC.PRG     (C102119,102130)
*: Program desc. : PRINT ORDER CONFIRMATION FOR SOC100.
*: Module        : Aria Apparel Series.
*: Developer     : Mohamed Shokry -(MHM)
*: Date          : 02/14/2001
*****************************************************************************
*: Calls : 
*:         Functions  : lfGetOrdLn,lfHeader,lfNotPad,lfBodyHdr,lfPrnLin
*:                      lfPrnTot
*****************************************************************************
*: Passed Parameters  : None
*****************************************************************************
*: Modifications :
*:C102119,4 some Rework Come From testing
*:B606908,1 SSE 01/23/2003 Fix bug of not printing company name.
*:B606991,1 ABD 03/20/2003 The customer sales order confimation is not 
*:B606991,1 ABD            Printing the alternate address if the order 
*:B606991,1 ABD            Ship to address is alternate.
*****************************************************************************

*---Variable Declaration
*:C102119,102130,4 change collecting data [start]
lcOrdFile = gfTempName()
CREATE CURSOR (lcOrdFile ) (ORDER C(6) )
*:C102119,102130,4 change collecting data [end]

llchkfile = .F.
*:C102119,102130,4 [start]
STORE '' TO lcStyMjr,lcClrMjr,lcColorNm
STORE 0 TO lnClrLen,lnClrPos,lnStyLen
*--Get color 
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lnStyLen = LEN(gfItemMask("PM"))

*:C102119,102130,4 [end]
*--change collecting data
STORE '' TO lcOldOrder 
*--To get orders from order temp file [start]

*lnPosition = ASUBSCRIPT(laOGVrFlt,ASCAN(laOGVrFlt,'ORDHDR.ORDER'),1)
*IF lnPosition > 0
*  lcOrdFile = laOGVrFlt[lnPosition,6]
*  IF !EMPTY(lcOrdFile) AND USED(lcOrdFile)
*    SELECT (lcOrdFile)
*    LOCATE
*    llchkfile = .F.
*  ELSE
*    *--if you did not choose any we will get all selected record from filter
*    CREATE CURSOR (lcOrdFile)  (ORDER C(6))
*    SELECT (lcTempOrd)
*    LOCATE
*    lcOldOrd = ''
*    llchkfile = .T.
*    SCAN 
*      IF lcOldOrd # &lcTempOrd..Order
*        SELECT (lcOrdFile)
*        APPEND BLANK
*        REPLACE ORDER WITH &lcTempOrd..Order
*        lcOldOrd = &lcTempOrd..Order
*      ENDIF  
*    ENDSCAN
*  ENDIF  
*ENDIF
SELECT (lcTempOrd)
LOCATE
IF EOF()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  *-- Variable to do EndReport Or Don't.
  llNoRec = .T.
  RETURN
ELSE
  *-- Variable to do EndReport Or Don't.
  llNoRec = .F.
ENDIF

SCAN 
  IF lcOldOrder # &lcTempOrd..Order
    INSERT INTO (lcOrdFile) (ORDER ) VALUES (&lcTempOrd..Order)
    lcOldOrder = &lcTempOrd..Order
  ENDIF  
ENDSCAN
*--To get orders from order temp file [end]

A= REPLICATE('*',80)
*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123
C='      GRP STYLE        COLOR                         PRICE  PIECES       AMOUNT'

*B606908,1 Keep the first address as it is. [Begin]
*laCompAdd1 = ''
laCompAdd1 = SPACE(ROUND((80-LEN(laCompAdd[1]))/2,0))+laCompAdd[1]
lcCompFax = SPACE(ROUND((80-LEN(ALLTRIM(lcCompFax)))/2,0))+lcCompFax
*B606908,1 Keep the first address as it is. [End]

laCompAdd2 =SPACE(ROUND((80-LEN(laCompAdd[2]))/2,0))+laCompAdd[2]
laCompAdd3 =SPACE(ROUND((80-LEN(laCompAdd[3]))/2,0))+laCompAdd[3]
laCompAdd4 =SPACE(ROUND((80-LEN(laCompAdd[4]))/2,0))+laCompAdd[4]
laCompAdd5 =SPACE(ROUND((80-LEN(laCompAdd[5]))/2,0))+laCompAdd[5]
lcLStore = ''
MAXROW = 49
XAMOUNT = 0
XTOTQTY = 0
XTOTORD = 0
ROW    = 0 
lnPage = 0
lcStyle = SPACE(12)
XTERM_DATA = ''
XSHIP_DATA = ''
XSPCI_DATA = ''
XDIVI_DATA = ''
XSEAS_DATA = ''
lcStore  = ''
XBTNAME  = ''
XBTADDR1 = ''
XBTADDR2 = ''
XBTADDR3 = ''
XSTNAME  = ''
XSTADDR1 = ''
XSTADDR2 = ''
XSTADDR3 = ''
XSTNAME  = ''
XSTADDR1 = ''
XSTADDR2 = ''
XSTADDR3 = ''
=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCK_LIN','SH')
=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDR','SH')
*-- Temporary files

lcOrdLTmp = gfTempName()
=lfGetOrdLn()

SET DEVICE TO PRINT
WAIT WINDOW 'PRINTING - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

SELECT (lcOrdFile)
LOCATE
SCAN WHILE INKEY() <>32
  lnPage = 0
  lcOrder = Order  
  =SEEK(lcOrder,lcOrdLTmp)
  =SEEK('O' + lcOrder,'OrdHdr')
  lcAccount = OrdHdr.Account
  llMulti   = (OrdHdr.MULTI = 'Y')
  =lfHeader()
  IF llRpOrdNot
    =lfNotPad()
  ENDIF
  =lfBodyHdr()
  =lfPrnLin()
ENDSCAN
IF llchkfile
  SELECT (lcOrdFile)
  ZAP
ENDIF
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfGetOrdLn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/14/2001
*! Purpose   : Colect Order line.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetOrdLn()
*!*************************************************************
FUNCTION lfGetOrdLn
SELE ORDLINE
= AFIELDS(laStrArr)
*--- Field for Department
lnStrArr = ALEN(laStrArr,1)
DIMENSION laStrArr[lnStrArr+1,4]
laStrArr[lnStrArr+1,1]='cSort'
laStrArr[lnStrArr+1,2]='C'
laStrArr[lnStrArr+1,3]='1'
laStrArr[lnStrArr+1,4]='0'
CREATE TABLE (gcWorkDir+lcOrdLTmp) FROM ARRAY laStrArr
SELECT (lcOrdFile)
LOCATE
SCAN
  lcOrder = Order
  SELECT OrdLine
  =SEEK('O' + lcOrder)
  WAIT WINDOW 'Colect order lines for order ..'+lcOrder NOWAIT
  SCAN REST WHILE 'O' + ORDER +STR(LINENO,6) =  'O' + lcOrder
    SCAT MEMVAR MEMO
    =SEEK(Style,'Style')
    DO CASE
      CASE Style.cStyGroup = 'PA'
        m.cSort = '1'
      CASE Style.cStyGroup = 'JY'
        m.cSort = '2'
      CASE Style.cStyGroup = 'SH'
        m.cSort = '3'
      CASE Style.cStyGroup = 'SO'
        m.cSort = '4'
      OTHERWISE
        m.cSort = '~'
    ENDCASE
    INSERT INTO (lcOrdLTmp) FROM MEMVAR
  ENDSCAN
ENDSCAN
SELECT(lcOrdLTmp)
*:C102119,4 MHM work sort by line no[start]
*IF llPack
*  INDEX ON ORDER+STORE+cSort+STYLE+STR(LINENO,6)+PACK_ID TAG &lcOrdLTmp
*ELSE 
*  INDEX ON ORDER+STORE+cSort+STYLE+STR(LINENO,6) TAG &lcOrdLTmp
*ENDIF
*--MHM2000 COMMENT AS PER AMIN
*IF llPack
*  IF lcRpSortBy = 'S'
*    INDEX ON ORDER+STORE+cSort+STYLE+STR(LINENO,6)+PACK_ID TAG &lcOrdLTmp
*  ELSE
*    INDEX ON ORDER+STORE+cSort+STR(LINENO,6)+STYLE+PACK_ID TAG &lcOrdLTmp
*  ENDIF
*ELSE
*--MHM2000 COMMENT AS PER AMIN
  IF lcRpSortBy = 'S'
    INDEX ON ORDER+STORE+cSort+STYLE+STR(LINENO,6) TAG &lcOrdLTmp
  ELSE
    INDEX ON ORDER+STORE+cSort+STR(LINENO,6)+STYLE TAG &lcOrdLTmp
  ENDIF
*--MHM2000 COMMENT AS PER AMIN
*ENDIF
*--MHM2000 COMMENT AS PER AMIN
*:C102119,4 MHM work sort by line no[END]
LOCATE

*!*************************************************************
*! Name      : lfHeader
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/14/2001
*! Purpose   : Colect header data and print header.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfHeader()
*!*************************************************************
FUNCTION lfHeader

XTERM_DATA = gfCodDes(ORDHDR.cTermCode, 'CTERMCODE '  )

XSHIP_DATA = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )

XSPCI_DATA = gfCodDes(ORDHDR.SpcInst , 'SPCINST   '  )

*--get division long name
*XDIVI_DATA = gfCodDes(ORDHDR.cDivision, 'CDIVISION '  )
XDIVI_DATA = ''
DIMENSION laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'XDIVI_DATA'
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

XSEAS_DATA = gfCodDes(ORDHDR.Season, 'SEASON    '  )

*B606908,1 Replace lcCompName with this value. [Begin]
*laCompAdd1 = IIF( !EMPTY(XDIVI_DATA) , XDIVI_DATA, gcCom_Name)
*laCompAdd1  = SPACE(ROUND((80-LEN( ALLTRIM(laCompAdd1) ))/2,0))+laCompAdd1
lcCompName = IIF( !EMPTY(XDIVI_DATA) , XDIVI_DATA, gcCom_Name)
lcCompName  = SPACE(ROUND((80-LEN( ALLTRIM(lcCompName) ))/2,0))+lcCompName
*B606908,1 Replace lcCompName with this value. [End]

SELECT(lcOrdLTmp)
IF llMulti
  lcStore = STORE
  SELECT CUSTOMER
  =SEEK ('S'+lcAccount+lcStore)
  XBTNAME  = BTNAME
  XBTADDR1 = cAddress1
  XBTADDR2 = cAddress2
  XBTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,15)) + ' ' + LEFT(cAddress5,10)
  IF LEN(TRIM(XBTADDR2)) =0
    XBTADDR2 = XBTADDR3
    XBTADDR3 = ''
  ENDIF
  XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
  XSTADDR1 = cAddress12
  XSTADDR2 = cAddress22
  XSTADDR3 = TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,5)) + ' ' + LEFT(cAddress52,10)
  IF LEN(TRIM(XSTADDR2)) =0
    XSTADDR2 = XSTADDR3
    XSTADDR3 = ''
  ENDIF
ELSE
  SELECT CUSTOMER
  =SEEK(IIF(ORDHDR.STORE=SPACE(8),'M'+lcAccount,'S'+lcAccount+ORDHDR.STORE))
  XBTNAME  = BTNAME
  XBTADDR1 = cAddress1
  XBTADDR2 = cAddress2
  XBTADDR3 = TRIM(LEFT(cAddress3,15)) + ' ' +TRIM(LEFT(cAddress4,15)) + ' ' + LEFT(cAddress5,10)
  IF LEN(TRIM(XBTADDR2)) =0
    XBTADDR2 = XBTADDR3
    XBTADDR3 = ''
  ENDIF
  lcStore = STORE
  *-- Initialize the alt address if ther is any. 04/07/94 MFM.
  SELECT OrdHdr
  =SEEK('O' + lcOrder)
  IF Alt_ShpTo
    *B606991,1 ABD The customer sales order confimation is not 
    *B606991,1 ABD Printing the alternate address if the order 
    *B606991,1 ABD Ship to address is alternate. [Begin]
    *SELECT Customer
    *XSTNAME  = STNAME
    *XSTADDR1 = cAddress12
    *XSTADDR2 = cAddress22
    *XSTADDR3 = TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,5)) + ' ' + LEFT(cAddress52,10)
    *IF LEN(TRIM(XSTADDR2)) =0
    * XSTADDR2 = XSTADDR3
    * XSTADDR3 = ''
    *ENDIF
    SELECT ORDHDR
    XSTNAME  = STNAME
    XSTADDR1 = cAddress1
    XSTADDR2 = cAddress2
    XSTADDR3 = ALLTRIM(cAddress3) + ' ' + ALLTRIM(cAddress4) + ' ' + ALLTRIM(cAddress5)
    IF LEN(TRIM(XSTADDR2)) =0
     XSTADDR2 = XSTADDR3
     XSTADDR3 = ''
    ENDIF
    *B606991,1 ABD [End]
  ELSE
    SELECT Customer
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = cAddress12
    XSTADDR2 = cAddress22
    XSTADDR3 = TRIM(LEFT(cAddress32,15)) + ' ' +TRIM(LEFT(cAddress42,5)) + ' ' + LEFT(cAddress52,10)
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
ENDIF

*B606908,1 Include the company name. [Begin]
*@ 01,01 SAY laCompAdd1 
*@ 02,01 SAY laCompAdd2 
*@ 03,01 SAY laCompAdd3
*@ 04,01 SAY laCompAdd4
*@ 05,01 SAY laCompAdd5 
@ 01,01 SAY lcCompName 
@ 02,01 SAY laCompAdd1
@ 03,01 SAY laCompAdd2
@ 04,01 SAY IIF(EMPTY(laCompAdd3),lcCompFax,laCompAdd3)
@ 05,01 SAY IIF(EMPTY(laCompAdd3),'',lcCompFax)
*B606908,1 Include the company name. [End]

@ 06,03 SAY 'PAGE  :'
lnPage = lnPage + 1
@ 06,11 SAY ALLTRIM(STR(lnPage,3))
@ 07,24 SAY 'INTERNAL PICKING SLIP (BULK ORDERS)'
SELECT ORDHDR        
@ 09,03 SAY 'ORDER: '+ORDER
@ 09,60 SAY 'DATE: '
@ 09,66 SAY ENTERED
@ 10,60 SAY 'PO #: '
@ 10,66 SAY IIF(MultiPO,&lcOrdLTmp..CUSTPO,CUSTPO) 
@ 11,45 SAY '********************************'
@ 12,04 SAY '........ SOLD TO ........'
@ 12,45 SAY '*........ SHIP TO ........     *'
@ 13,04 SAY ACCOUNT
lcStore='STORE: ' + lcStore + '  DEPT: ' + DEPT
LCSTORE1 =  "*"+lcStore+IIF(LEN(lcStore)<30,SPACE(30-LEN(lcStore))+"*","*")
@ 13,45 SAY LCSTORE1
SET DEVICE TO SCREEN
WAIT WINDOW 'ORDER : '+Order+'  '+lcStore NOWAIT
SET DEVICE TO PRINT
@ 14,04 SAY XBTNAME
@ 14,45 SAY "*"+XSTNAME+IIF(LEN(XSTNAME)<30,SPACE(30-LEN(XSTNAME))+"*","*")
@ 15,04 SAY XBTADDR1
@ 15,45 SAY "*"+XSTADDR1+IIF(LEN(XSTADDR1)<30,SPACE(30-LEN(XSTADDR1))+"*","*")
@ 16,04 SAY XBTADDR2
@ 16,45 SAY "*"+XSTADDR2+IIF(LEN(XSTADDR2)<30,SPACE(30-LEN(XSTADDR2))+"*","*")
@ 17,04 SAY XBTADDR3
@ 17,45 SAY "*"+XSTADDR3+IIF(LEN(XSTADDR3)<30,SPACE(30-LEN(XSTADDR3))+"*","*")
@ 18,45 SAY '********************************'
@ 19,00 SAY A
@ 20,00 SAY B
@ 21,00 SAY A
@ 22,01 SAY START
@ 22,12 SAY COMPLETE
@ 22,22 SAY LEFT(XTERM_DATA,15)
@ 22,38 SAY LEFT(XSHIP_DATA,15)
@ 22,54 SAY LEFT(XSEAS_DATA,10)
@ 22,65 SAY REP1
@ 22,70 SAY REP2
@ 23,00 SAY A
ROW = 23

*!*************************************************************
*! Name      : lfNotPad
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/14/2001
*! Purpose   : Print NoePad.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfNotPad()
*!*************************************************************
FUNCTION lfNotPad
PRIVATE lnOldAlias

lnOldAlias = SELECT(0)
SELECT NOTEPAD
IF !SEEK('B'+lcOrder)
  RETURN
ENDIF
lnMline = 1
lnMemWidth = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
DO WHILE lnMline <= lnMTotLin
  SELECT NOTEPAD
  IF ROW >= MAXROW
    ROW = ROW+1
    @ROW,00 SAY REPLICATE('-',80)
    ROW = ROW+1
    @ROW,25 SAY "** C O N T I N U E D **"
    ROW = ROW+1
    @ROW,00 SAY REPLICATE('-',80)
    =lfHeader()
    SELECT NOTEPAD
  ENDIF
  IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
    ROW = ROW+1
    @ ROW,04 SAY MLINE(MNOTES,lnMline)
  ENDIF
  lnMline = lnMline + 1
ENDDO
SET MEMOWIDTH TO lnMemWidth
SELECT(lnOldAlias)

*!*************************************************************
*! Name      : lfBodyHdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/14/2001
*! Purpose   : Print body header.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfBodyHdr()
*!*************************************************************
FUNCTION lfBodyHdr

ROW = ROW + 1
@ROW,00 SAY REPLICATE('-',80)
ROW = ROW + 1
            *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
@ROW,00 SAY "Num.          Style                Color    Size   Qty       Shipped   BackOrder"
ROW = ROW + 1
@ROW,00 SAY REPLICATE('-',80)
ROW = ROW+1

IF lnPage=0
  lcStyle = SPACE(12)
ENDIF  

*!*************************************************************
*! Name      : lfPrnLin
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/14/2001
*! Purpose   : Print body .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnLin()
*!*************************************************************
FUNCTION lfPrnLin
PRIVATE lnOld

lnOld = SELECT(0)
SELECT (lcOrdLTmp)
DO WHILE !EOF() AND ORDER = lcOrder
  lcLStore = Store
  lcStyle = SPACE(12)
  SCAN REST WHILE ORDER+STORE+cSort+STYLE+STR(LINENO,6) = ;
                  lcOrder + lcLStore
    IF Style <> lcStyle
      IF !EMPTY(lcStyle)
        @ROW,00 SAY REPLICATE('-',80)
        ROW = ROW + 1
      ENDIF
      lcStyle = Style
    ENDIF
    lcKEY = STYLE 
    =SEEK(lcKEY,'STYLE')
    lcStyDesc = STYLE.DESC
    =SEEK('S'+STYLE.SCALE,'SCALE')
    lnIndex = 1
    DO WHILE lnIndex <= SCALE.Cnt
      lcIndex = ALLTRIM(STR(lnIndex,1))
      IF Qty&lcIndex = 0
        lnIndex = lnIndex + 1
        LOOP
      ENDIF
      
      *--*:C102119,102130,4 [start]
      *@ ROW,00 SAY STYLE
      *@ ROW,20 SAY LEFT(lcStyDesc,22)
      =lfStyMj(style)
      @ ROW,00 SAY lcStyMjr
      @ ROW,14 SAY LEFT(lcStyDesc,20)
      @ ROW,35 SAY lcClrMjr
      *:C102119,102130,4 [end]
      
      @ ROW,44 SAY SCALE.SZ&lcIndex
      @ ROW,51 SAY ALLTRIM(STR(Qty&lcIndex,5))
      @ ROW,61 SAY '--------'
      @ ROW,71 SAY '---------'
      ROW = ROW + 1
      IF ROW > MAXROW
        ROW = ROW+1
        @ROW,00 SAY REPLICATE('-',80)
        ROW = ROW+1
        @ROW,25 SAY "** C O N T I N U E D **"
        ROW = ROW+1
        @ROW,00 SAY REPLICATE('-',80)
        =lfHeader()
        =lfBodyHdr()
        SELECT (lcOrdLTmp)
      ENDIF
        lnIndex = lnIndex + 1
    ENDDO
    *--MHM2000 COMMENT AS PER AMIN
    *IF llPack
    *  lcOldAlias = ALIAS()
    *  SELECT SPCK_LIN
    *  SET ORDER TO SPCKLINS
    *  =SEEK('P'+lcAccount+&lcOrdLTmp..Style+&lcOrdLTmp..PACK_ID)
    *  SELECT (lcOldAlias)
    *  IF !EMPTY(&lcOrdLTmp..PACK_ID)
    *    @ ROW,00 SAY 'PACK_ID : '+&lcOrdLTmp..PACK_ID+'  '+'DESC.:'+SPCK_HDR.DESC+' ' +;
    *                 +STR(SPCK_LIN.Qty1,3)+' '+ STR(SPCK_LIN.Qty2,3)+' '+STR(SPCK_LIN.Qty3,3)+' ' +;
    *                  STR(SPCK_LIN.Qty4,3)+' '+STR(SPCK_LIN.Qty5,3)+' '+STR(SPCK_LIN.Qty6,3)
    *    ROW = ROW + 1
    *  ENDIF
    *ENDIF
    *--MHM2000 COMMENT AS PER AMIN
    XAMOUNT = TOTQTY * PRICE
    XTOTQTY = XTOTQTY+ TOTQTY
    XTOTORD = XTOTORD+ XAMOUNT
  ENDSCAN
  =lfPrnTot()
  IF !EOF() AND ORDER = lcOrder
    =lfHeader()
    IF llRpOrdNot
      =lfNotPad()
    ENDIF
    =lfBodyHdr()
    *:C102119,4[Start]
    *ROW = ROW + 1
    *:C102119,4 [End]
  ENDIF
  SELECT (lcOrdLTmp)
ENDDO

SELECT(lnOld)

*!*************************************************************
*! Name      : lfPrnTot
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/14/2001
*! Purpose   : Print Store total.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPrnTot()
*!*************************************************************
FUNCTION lfPrnTot

lnPage = 0
XMER_DISC = ORDHDR.DISC
XMER_AMNT = (XMER_DISC*XTOTORD)/100
XNET_AMNT = XTOTORD - XMER_AMNT
@ 52,00 SAY REPLICATE('-',80)
@ 53,51 SAY  'GROSS AMOUNT  =>'
@ 53,69 SAY XTOTORD PICTURE '999,999.99'
@ 54,51 SAY  'DISCOUNT      =>'
@ 54,69 SAY XMER_AMNT PICTURE '999,999.99'
@ 55,51 SAY  'NET AMOUNT    =>'
@ 55,69 SAY XNET_AMNT PICTURE '999,999.99'
@ 56,51 SAY  'TAX           =>'
@ 56,69 SAY  XNET_AMNT*(CUSTOMER.nTaxRate/100) PICTURE '999,999.99'
@ 57,51 SAY  'TOTAL         =>'
lnTax = XNET_AMNT*(CUSTOMER.nTaxRate/100)
@ 57,69 SAY (XNET_AMNT+lnTax) PICTURE '999,999.99'
@ 58,22 SAY '______________________________________   ____________'
@ 59,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
XTOTQTY  = 0.00
XTOTORD  = 0.00
*!*************************************************************
*! Name      : lfStyMj                   (C102119,102130,4)
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/14/2001
*! Purpose   : get style and style color.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfStyMj()
*!*************************************************************
FUNCTION lfStyMj
PARAMETERS lcStyMaj
lcClrMjr = SUBSTR(lcStyMaj,lnClrPos,lnClrLen)
lcStyMjr = SUBSTR(lcStyMaj,1,lnStyLen)
