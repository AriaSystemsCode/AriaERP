*:***************************************************************************
*: Program file  : ALPKTKLS.PRG
*: Program desc. : Custom Picking ticket form for (BONG HWA).
*: Date          : 08/19/2003
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Tracking #    : C102844,1
*:***************************************************************************
*: Example : DO ALPKTKLS
*:***************************************************************************
*: Modifications
*: B122140,1 NNA 03/16/2004 fix bug of that the styles lines Overwrite the Piktkt Footer (Remove 5 lines)
*: B122140,1 NNA            and insert an empty line between item lines
*: B123033,1 ASH 05/25/2004 Don't print Required and B.O. qty.
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

SELECT (lcTmpOrdL)
SET FILTER TO cGrupDetal = "D"
LOCATE
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

=lfPrnLines()
llNoRec = .F.

*!**************************************************************************
*! Name      : lfPrnLines
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
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
  ldEntrdDt  = OrdHdr.ENTERED
  lcTerms    = OrdHdr.CTERMCODE 
  lcShipVia  = IIF( ORDHDR.MULTI = 'Y' , lfShipVia() , OrdHdr.ShipVia )
  lcRep1     = OrdHdr.Rep1
 
  =lfPrintHdr()
  STORE 0 TO lnTotPik, lnTotPrice
  lnPgNo = 1
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6) = lcPickTkt+lcOrder+'D'
    *B122140,1 NNA (Begin) 03/16/2004 Remove five lines from the form to not overwrite the form footer
    *IF lnRow > 56
    IF lnRow > 51
    *B122140,1 NNA (End)
      =lfPrnContI()
      =lfPrintHdr()
    ENDIF
    
    lcClr = SUBSTR(Style,lnNonMajSt,lnColorLen)
    *B123033,1 ASH 05/25/2004 (Begin) Don't print Required qty.
    *@ lnRow, 2  SAY ORDLINE.TOTBOOK        PICTURE '99999'
    *B123033,1 ASH 05/25/2004 (End)
    @ lnRow, 8  SAY TOTPIK        PICTURE '99999'
    *B123033,1 ASH 05/25/2004 (Begin) Don't print B.O. qty.
    *@ lnRow, 14 SAY IIF(ORDLINE.TOTBOOK=TOTPIK,0,ORDLINE.TOTBOOK-TOTPIK) PICTURE '99999'    
    *B123033,1 ASH 05/25/2004 (End)
    @ lnRow, 22 SAY SUBSTR(ALLTRIM(SUBSTR(STYLE,1,lnMajorLen))+ALLTRIM(lcClr) , 1 , 10 )
    @ lnRow, 33 SAY SUBSTR(IIF(!EMPTY(STYLE.PREPAK),STYLE.PREPAK+'-','')+ALLTRIM(gfCodDes(lcClr,'COLOR     '))+' '+;
                    ALLTRIM(SUBSTR(OrdLine.DESC1,1,22)),1,27)
    @ lnRow, 60 SAY PRICE          PICTURE  '99999.99'
    @ lnRow, 71 SAY TOTPIK * PRICE PICTURE '999999.99'
    *B122140,1 NNA 03/16/2004 (Begin) Add an Empty line between the items
    *lnRow = lnRow + 1
    lnRow = lnRow + 2    
    *B122140,1 NNA (End)
    lnTotPik   = lnTotPik + TotPik
    lnTotPrice = lnTotPrice + (TotPik * Price)
    
  ENDSCAN

  IF SEEK ('B' + lcOrder,'NotePad')
    =lfPrnNotes()
  ENDIF
    
  =lfPrntTot()
ENDDO
WAIT CLEAR

*!**************************************************************************
*! Name      : lfPrintHdr
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
*! Purpose   : To print the header of the picking ticket.
*!**************************************************************************
*! Example   :  =lfPrintHdr()
*!**************************************************************************
FUNCTION lfPrintHdr
PRIVATE lnAlias

lnAlias = SELECT(0)
=lfCustInfo()

@ 02 , 71 SAY lcAccount
@ 05 , 56 SAY "PICKING TICKET #:"+lcPickTkt
@ 08 , 54 SAY ALLTRIM(SUBSTR(lcStore,1,8))

@ 09 , 09 SAY lcBtName
@ 09 , 53 SAY lcStName
@ 10 , 09 SAY lcBtAdd1
@ 10 , 53 SAY lcStAdd1
@ 11 , 09 SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd2,lcBtAdd3)
@ 11 , 53 SAY IIF(!EMPTY(lcStAdd2),lcStAdd2,lcStAdd3)
@ 12 , 09 SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd3,'')
@ 12 , 53 SAY IIF(!EMPTY(lcStAdd2),lcStAdd3,' ')

@ 18 , 02 SAY gdSysDate
@ 18 , 12 SAY ALLTRIM(gfCodDes(lcShipVia,'SHIPVIA  '))
@ 18 , 51 SAY ALLTRIM(gfCodDes(lcTerms,'CTERMCODE '))
@ 20 , 02 SAY ALLTRIM(lcCustPO)
@ 20 , 22 SAY ldEntrdDt
@ 20 , 36 SAY ALLTRIM(lcRep1)
@ 20 , 67 SAY lcOrder

lnRow = 23

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrnNotes
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
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
   IF lnRow > 56
     =lfPrnContI()
     =lfPrintHdr()
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
*! Name      : lfPrntTot
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
*! Purpose   : To print the totals
*!**************************************************************************
*! Example   :  =lfPrntTot()
*!**************************************************************************
FUNCTION lfPrntTot
@ 57, 58 SAY "TOTAL ORDER " + TRANSFORM(lnTotPrice,'999999.99')
@ 59, 70 SAY 'Page '+ALLTRIM(STR(lnPgNo))


*!**************************************************************************
*! Name      : lfPrnContI
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
*! Purpose   : To print the continue word.
*!**************************************************************************
*! Example   :  =lfPrnContI()
*!**************************************************************************
FUNCTION lfPrnContI
@ 59, 3 SAY '* C O N T I N U E D *'
@ 59, 70 SAY 'Page '+ALLTRIM(STR(lnPgNo))
lnPgNo = lnPgNo + 1

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : TMI - TAREK MOHAMED IBRAHIM
*: Date        : 08/24/2003
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
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 08/24/2003
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
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    =SEEK('S' + &lcTmpOrdH..Account + lcDistCntr)
  ELSE
    =SEEK(IIF(EMPTY(&lcTmpOrdL..Store) , 'M'+ &lcTmpOrdH..Account,'S' + &lcTmpOrdH..Account + &lcTmpOrdL..Store))
  ENDIF

  lcStName = Customer.STNAME
  =gfGetAdr('Customer','','','',1 )

  *--Get the Ship To address.
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    lcStAdd&lcCount = ALLTRIM(lcStAdd&lcCount) + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  	
ENDIF  
SELECT (lcAlias)

*:**************************************************************************
*:* Name        : lfShipVia
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/02/2003
*:* Purpose     : Get ship via for each store in multi store case
*:***************************************************************************
FUNCTION lfShipVia
PRIVATE lnSlct,lcCusExp,lcShipVia
lnSlct = SELECT()

SELECT CUSTOMER
=SEEK('S'+lcAccount+lcStore,'Customer')
IF CUSTOMER.nBrkWeight <> 0 .AND. &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight
  lcShipVia = CUSTOMER.cAltShpvia
ELSE  
  lcShipVia = CUSTOMER.SHIPVIA
ENDIF  

SELECT (lnSlct)
RETURN lcShipVia
*-- end of lfShipVia.