*:***************************************************************************
*: Program file  : ARPINVBN.PRG (Custom Invoice for for Bong)
*: Program desc. : Invoice Form
*: Date          : 08/31/2003
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Mohamed Shokry(MHM)
*: Tracking Job Number: 102843
*:***************************************************************************
*: Example : DO ARPINVBN.PRG
*:***************************************************************************
*: Modifications
*: B121578,1 ASH 02/11/2004 Add some text in case of factor invoices.
*: B122854,1 BWA 20/05/2004 Fix the calculatiing od the Amount to Pay.
*: C129343,1 MMR 09/18/2005 Adding discounted amount per line.
*: C129343,2 MMR 02/02/2006  1)Replace the discount price column with the original unit price.
*: C129343,2 MMR             2)Replace the discounted amount column with the discounted price. 
*: C129343,2 MMR             3)Replace the DISC Field with the total disc amount. 
*: C129343,3 AHU             1)Replace the DISC Field with the total disc amount plus the header disc. 
*: C129343,3 AHU             2)Replace the shipped  Field with the total original unit price. 
*: C200820,1 TMI 07/19/2007 Allow to print the factor address variablly
*: C200874,1 NNA 10/17/2007 Display a new Invoice form message if The factor name is CAPITAL FACTORS, INC. 
*:***************************************************************************

STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst
*C129343,2 MMR 02/02/2006  3)Replace the DISC Field with the total disc amount.[Start] 
lnTotLinDisc = 0
*C129343,2 MMR.[End]
=lfEvalSegs()

llWareHous = (ALLTRIM(UPPER(gfGetMemVar('M_WAREHOUS'))) = 'Y')
lcWhName = lcCompName

IF llWareHous
   =gfOpenFile(gcDataDir+'WareHous','WareHous','SH')
ENDIF

=gfOpenFile(gcDataDir+'OrdLine','OrdLine','SH')

STORE 0 TO lnRow

DIMENSION laScales[4], laAddress[1,1]
STORE '' TO laScales, laAddress
*- Ship to and Bill to Addresses
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6

STORE .F. TO llPrint
STORE 1 TO ln1stCopy, ln2ndCopy
STORE '' TO lc1stPort, lc2ndPort


SELECT INVHDR
lcRpExp = STRTRAN(lcRpExp,"INVHDR.","")
lcRpExp = "INVOICE = '' AND " + lcRpExp
COPY ALL FOR &lcRpExp TO &gcWorkDir.&lcDumFile

IF !USED(lcDumFile)
  *-- there is record in SYREPUVR file for gfTempName called lcDumFile
  *-- this will be used to carry the INVOICE HEADER TEMP. File
  = gfOpenFile('&gcWorkDir.&lcDumFile',' ','EX')
ENDIF  

SELECT &lcDumFile
SET RELATION TO INVOICE INTO INVHDR ADDITIVE

SELECT INVHDR
SET RELATION TO
SET RELATION TO IIF(CONSOL='Y',INVOICE,'') INTO CONSINVH ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
IF llWareHous
   SET RELATION TO CWARECODE INTO WAREHOUS ADDITIVE
ENDIF

IF RECCOUNT(lcDumFile) = 0
  *--No records to display.
  llNoRec = .T.
  = gfModalGen('TRM00052B00000','DIALOG' )
  IF USED(lcDumFile)
    USE IN (lcDumFile)
  ENDIF
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
  RETURN
ENDIF

=lfPrevPrn()
llNoRec = .F.

*-- Close Temporary file [Begin.]
IF USED(lcDumFile)
  USE IN (lcDumFile)
ENDIF  
IF FILE(gcWorkDir+lcDumFile+'.DBF')
  ERASE &gcWorkDir.&lcDumFile+'.DBF'
ENDIF  
*-- Close Temporary file [End.]

SET DEVICE TO SCREEN
*!**************************************************************************
*! Name      : lfPrintHdr
*: Developer : Mohamed Shokry(MHM)
*! Date      : 08/31/2003
*! Purpose   : To print the header of the picking ticket.
*!**************************************************************************
*! Example   :  =lfPrintHdr()
*!**************************************************************************
FUNCTION lfPrintHdr
PRIVATE lnAlias

lnAlias = SELECT(0)
=lfCustInfo()

@ 0, 70 SAY InvHdr.Invoice
@ 2, 70 SAY CUSTOMER.Account


IF !EMPTY(INVHDR.STORE)
	@ 8, 53 SAY INVHDR.STORE
	@ 9, 09 SAY lcBtName
ELSE	
	@ 9, 09 SAY lcBtName
	@ 9, 53 SAY lcStName
ENDIF

@ 10 , 09 SAY lcBtAdd1
@ 10 , 53 SAY lcStAdd1
@ 11 , 09 SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd2,lcBtAdd3)
@ 11 , 53 SAY IIF(!EMPTY(lcStAdd2),lcStAdd2,lcStAdd3)
@ 12 , 09 SAY IIF(!EMPTY(lcBtAdd2),lcBtAdd3,'')
@ 12 , 53 SAY IIF(!EMPTY(lcStAdd2),lcStAdd3,' ')

@ 18 , 02 SAY InvHdr.InvDate

@ 18 , 12 SAY ALLTRIM(gfCodDes(InvHdr.ShipVia,'SHIPVIA  '))
@ 18 , 51 SAY ALLTRIM(gfCodDes(InvHdr.cTermCode,'CTERMCODE '))

@ 20 , 02 SAY ALLTRIM(INVHDR.CustPo)
@ 20 , 22 SAY ORDHDR.Entered
@ 20 , 36 SAY ALLTRIM(INVHDR.Rep1)
@ 20 , 67 SAY INVHDR.Order

lnRow = 23

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfPrnLines
*: Developer : Mohamed Shokry(MHM)
*! Date      : 08/31/2003
*! Purpose   : To print the invoice lines
*!**************************************************************************
*! Example   :  =lfPrnLines()
*!**************************************************************************
FUNCTION lfPrnLines
PRIVATE lnAlias
lnAlias = SELECT(0)

IF InvHdr.CONSOL = 'Y'
   SELECT CONSINVL
ELSE
   SELECT INVLINE
ENDIF

STORE 0 TO lnTotAmt, lnTotShip
=SEEK(&lcDumFile..Invoice)

llContNu = .F.
lnPgNo = 1

SCAN REST WHILE Invoice+STR(LineNo,6) = &lcDumFile..Invoice
  WAIT WINDOW NOWAIT 'Style-Color :'+ALLTRIM(STYLE)

  IF lnRow > 48
    = lfPrnContI()
    = lfPrintHdr()
   ENDIF

  =Seek("O"+InvLine.Order+Str(InvLine.LineNo,6),"OrdLine")

  @ lnRow, 2 SAY OrdLine.TotBook PICTURE '99999'
  @ lnRow, 8 SAY InvLine.TotQty PICTURE '99999'
  @ lnRow, 14 SAY OrdLine.TotQty PICTURE '999999'
  
  @ lnRow, 22 SAY LEFT(ALLTRIM(SUBSTR(STYLE,1,lnMajorLen))+ ALLTRIM(SUBSTR(Style,lnNonMajSt,lnColorLen)),10)
  *C129343,1 MMR 09/18/2005 Adding discounted amount per line.[Start]
  *@ lnRow, 33 SAY LEFT(IIF(!EMPTY(STYLE.PREPAK),STYLE.PREPAK+'-','')+ALLTRIM(gfCodDes(ALLTRIM(SUBSTR(Style,lnNonMajSt,lnColorLen)),'COLOR  '))+"-"+ALLTRIM(SUBSTR(OrdLine.DESC1,1,22)),27)
  *@ lnRow, 60 SAY PRICE PICTURE  '99999.99'
  *@ lnRow, 71 SAY TOTQTY*PRICE PICTURE '999999.99'

  @ lnRow, 33 SAY LEFT(IIF(!EMPTY(STYLE.PREPAK),STYLE.PREPAK+'-','')+ALLTRIM(gfCodDes(ALLTRIM(SUBSTR(Style,lnNonMajSt,lnColorLen)),'COLOR  '))+"-"+ALLTRIM(SUBSTR(OrdLine.DESC1,1,22)),19)
  *C129343,2 MMR 02/02/2006  1)Replace the discount price column with the original unit price.
  *C129343,2 MMR             2)Replace the discounted amount column with the discounted price. 
  *@ lnRow, 53 SAY PRICE PICTURE  '99999.99'
  *@ lnRow, 62 SAY (TOTQTY*GROS_PRICE*DISC_PCNT)/100 PICTURE  '99999.99'
  @ lnRow, 53 SAY GROS_PRICE PICTURE  '99999.99'
  @ lnRow, 62 SAY PRICE PICTURE  '99999.99'
  *C129343,2 MMR.[End] 
  @ lnRow, 71 SAY TOTQTY*PRICE PICTURE '999999.99'
  *C129343,1 MMR.[End]
  lnRow = lnRow + 1
*C129343,2 MMR 02/02/2006  3)Replace the DISC Field with the total disc amount.[Start] 
lnTotLinDisc = lnTotLinDisc + ((TOTQTY*GROS_PRICE*DISC_PCNT)/100)
*C129343,2 MMR.[End] 
*  lnTotShp   = lnTotShp + TotQty
*  lnTotPrice = lnTotPrice + (TotQty*Price)

ENDSCAN
IF llRpInvNot AND SEEK ('B' + InvHdr.Order,'NotePad')
	=lfPrnNotes()
ENDIF

IF llRpInvNot AND SEEK ('C' + InvHdr.Invoice,'NotePad')
	=lfPrnNotes()
ENDIF
*B121578,1 ASH 02/11/2004 (Begin) Add some text in case of factor invoices.
IF !EMPTY(InvHdr.cFacCode)
  = lfPrnFac()
ENDIF
*B121578,1 ASH 02/11/2004 (End)
= lfPrntTot()
WAIT CLEAR

*!**************************************************************************
*! Name      : lfPrntTot
*: Developer : Mohamed Shokry(MHM)
*! Date      : 08/31/2003
*! Purpose   : To print the totals
*!**************************************************************************
*! Example   :  =lfPrntTot()
*!**************************************************************************
PROCEDURE lfPrntTot
*C200874,1 NNA 10/17/2007 (Begin) move the total section one row to blow
*!*	@ 50, 56 SAY 'Total     :'
*!*	@ 50, 67 SAY InvHdr.Ship PICTURE '99999999999'
*!*	@ 51, 56 SAY 'Shipped   :'
*!*	*: C129343,3 AHU             2)Replace the shipped  Field with the total original unit price. [Start]
*!*	*@ 51, 67 SAY InvHdr.ShipAmt PICTURE '99999999.99'
*!*	@ 51, 67 SAY (InvHdr.ShipAmt+lnTotLinDisc) PICTURE '99999999.99'
*!*	*: C129343,3 AHU [End]
*!*	@ 52, 56 SAY 'Disc      :'
*!*	*C129343,2 MMR 02/02/2006  3)Replace the DISC Field with the total disc amount.[Start] 
*!*	*@ 52, 67 SAY InvHdr.Discount PICTURE '99999999.99'
*!*	*@ 52, 67 SAY lnTotLinDisc PICTURE '99999999.99'
*!*	*: C129343,3 AHU      1)Replace the DISC Field with the total disc amount plus the header disc. [Start]
*!*	@ 52, 67 SAY (lnTotLinDisc-InvHdr.Discount) PICTURE '99999999.99'
*!*	lnTotLinDisc=0
*!*	*C129343,2 MMR.[End]
*!*	*C129343,3 AHU.[End]
*!*	*ash 10/31/03 Print the charges
*!*	@ 53, 56 SAY 'Charges   :'
*!*	@ 53, 67 SAY INVHDR.Freight + INVHDR.Insur + INVHDR.COD PICTURE '99999999.99'
*!*	@ 54, 56 SAY 'Amt To Pay:'
*!*	*B122854,1 BWA 20/05/2004 Fix the calculating od the Amount to Pay.[START]
*!*	*ash 10/31/03 Add the charges to invoice total.
*!*	*@ 54, 67 SAY InvHdr.ShipAmt + INVHDR.Freight + INVHDR.Insur + INVHDR.COD - InvHdr.Discount PICTURE '99999999.99'
*!*	@ 54, 67 SAY InvHdr.ShipAmt + INVHDR.Freight + INVHDR.Insur + INVHDR.COD + InvHdr.Discount PICTURE '99999999.99'
*!*	*B122854,1 BWA 20/05/2004.[END]
*!*	@ 59, 70 SAY 'Page '+ALLTRIM(STR(lnPgNo))

@ 51, 56 SAY 'Total     :'
@ 51, 67 SAY InvHdr.Ship PICTURE '99999999999'
@ 52, 56 SAY 'Shipped   :'
@ 52, 67 SAY (InvHdr.ShipAmt+lnTotLinDisc) PICTURE '99999999.99'
@ 53, 56 SAY 'Disc      :'
@ 53, 67 SAY (lnTotLinDisc-InvHdr.Discount) PICTURE '99999999.99'
lnTotLinDisc=0
@ 54, 56 SAY 'Charges   :'
@ 54, 67 SAY INVHDR.Freight + INVHDR.Insur + INVHDR.COD PICTURE '99999999.99'
@ 55, 56 SAY 'Amt To Pay:'
@ 55, 67 SAY InvHdr.ShipAmt + INVHDR.Freight + INVHDR.Insur + INVHDR.COD + InvHdr.Discount PICTURE '99999999.99'
@ 60, 70 SAY 'Page '+ALLTRIM(STR(lnPgNo))
*C200874,1 NNA (End)
*!**************************************************************************
*! Name      : lfPrnContI
*: Developer : Mohamed Shokry(MHM)
*! Date      : 08/31/2003
*! Purpose   : To print the continue word.
*!**************************************************************************
*! Example   :  =lfPrnContI()
*!**************************************************************************
FUNCTION lfPrnContI
@ 59, 3 SAY '* C O N T I N U E D *'
@ 59, 70 SAY 'Page '+ALLTRIM(STR(lnPgNo))
lnPgNo = lnPgNo + 1

*:***************************************************************************
*: Name        : lfPrevPrn
*: Developer   : Mohamed Shokry(MHM)
*! Date        : 08/31/2003
*: Purpose     : To print the invoices in case of preview
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*! Example     : = lfPrevPrn()
*:***************************************************************************
FUNCTION lfPrevPrn

CLEAR TYPEAHEAD
SET DEVICE TO PRINT
SELECT (lcDumFile)
GO TOP

SCAN REST WHILE INKEY() <> 32
  lcInvoice = InvHdr.Invoice    && store invoice no to be used in updating prtFlag field
  WAIT WINDOW 'PRINTING INVOICE ' + INVOICE + ' - <Space Bar> TO ABORT' NOWAIT
  =lfPrintHdr()
  =lfPrnLines()
*khalid
 IF VARREAD() = "PBRUN" AND SEEK(lcInvoice,'InvHdr')
   REPLACE InvHdr.PrtFlag WITH 'P'
 ENDIF  

ENDSCAN
WAIT CLEAR


*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Mohamed Shokry(MHM)
*! Date        : 08/31/2003
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
*: Developer : Mohamed Shokry(MHM)
*! Date      : 08/31/2003
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

lcCusExp   = IIF(EMPTY(&lcDumFile..Store),"M","S") + &lcDumFile..Account +;
             IIF(EMPTY(&lcDumFile..Store),"",&lcDumFile..Store)
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
  lcStAdd3 = ALLTRIM(OrdHdr.cAddress3) + ',' + ALLTRIM(OrdHdr.cAddress4)+','+ALLTRIM(OrdHdr.cAddress4)
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
*! Name      : lfPrnFac
*! Developer : ASH - Ashraf Sherif Mohammed
*! Date      : 02/11/2004
*! Purpose   : To print factor information
*! B121578,1 
*!**************************************************************************
*! Example   :  =lfPrnFac()
*!**************************************************************************
FUNCTION lfPrnFac
IF lnRow > 44
  =lfPrnContI()
  =lfPrintHdr()
ENDIF
*C200820,1 TMI [Start] Comminting the following lines 
*-* @ 44,1 SAY 'THIS ACCOUNT AND THE MERCHANDISE IT REPRESENTS HAS BEEN ASSIGNED TO AND IS'
*-* @ 45,1 SAY 'PAYABLE ONLY TO ROSENTHAL & ROSENTHAL, INC. P.O.BOX 88926, CHICAGO, IL '
*-* @ 46,1 SAY '60695-1926 MAKE CHECK PAYABLE AS ABOVE IN UNITED STATES FUNDS AND INDICATE'
*-* @ 47,1 SAY 'NAME OF SELLER. ANY CLAIMS AGAINST THIS INVOICE MUST BE MADE PROMPTLY IN '
*-* @ 48,1 SAY 'WRITING TO ROSENTHAL & ROSENTHAL,INC. ,SPECIFYING DETAILS. NO RETURNS OR'
*-* @ 49,1 SAY 'ADJUSTMENTS WILL BE RECOGNIZED WITHOUT THE WRITTEN CONSENT OF ROSENTHAL &'
*-* @ 50,1 SAY 'ROSENTHAL, INC.'

*- Open the sycfact system file
IF !USED('SYCFACT')
  =gfOpenFile(gcSysHome+'SYCFACT','CFACCODE')
ENDIF

*- Locate the factor name and address
PRIVATE lcFacName , lcFactAdd
=SEEK(INVHDR.CFACCODE,'SYCFACT')
lcFacName   = SYCFACT.cfaccomp
laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
=lfAdrShift('laFactor')
lcFactAdd  = ALLTRIM(laFactor[1])+IIF(!EMPTY(ALLTRIM(laFactor[1])),', ','')+;
             ALLTRIM(laFactor[2])+IIF(!EMPTY(ALLTRIM(laFactor[2])),', ','')+;
             AllTRIM(laFactor[3])+IIF(!EMPTY(allTRIM(laFactor[3])),', ','')+;
             ALLTRIM(laFactor[4])+IIF(!EMPTY(allTRIM(laFactor[4])),', ','')+;
             ALLTRIM(laFactor[5])
lcFactAdd = IIF(RIGHT(lcFactAdd,1)=',',LEFT(lcFactAdd,LEN(lcFactAdd)-1),lcFactAdd)             

*C200874,1 NNA 10/17/2007 (BEGIN) print another statment if the factor is CAPITAL FACTORS, INC
IF ALLTRIM(INVHDR.CFACCODE)=='CPT'
  *C200874,3 TMI [Start] I found that 4 lines of these are not compatible with the spcs , I commented them and rewritten 
  *                      them again as per the specs  
  *@ 44,1 SAY '        This bill and all future bills are assigned to, owned by and are payable only to:'
  *@ 45,1 SAY '                       CAPITAL FACTORS, INC. <= New Factor Info '
  *@ 46,1 SAY '                                     P O BOX 79 '
  *@ 47,1 SAY '                               MEMPHIS, TN 38101-0079 '
  *@ 48,1 SAY 'To whom prompt notice must be given of any merchandise returns and any claims or disputes whether based'
  *@ 49,1 SAY '                       on shortages, non-delivery, offsets or any other claim.'
  @ 44,1 SAY 'This bill and all future bills are assigned to, owned by and are payable only'
  @ 45,1 SAY PADC('To: CAPITAL FACTORS, INC.',80)
  @ 46,1 SAY PADC('P O BOX 79',80)
  @ 47,1 SAY PADC('MEMPHIS, TN 38101-0079',80)
  @ 48,1 SAY 'To whom prompt notice must be given of any merchandise returns and any Claims'
  @ 49,1 SAY 'Or disputes whether based on Shortages, non-delivery, offsets or '
  @ 50,1 SAY 'Other claims.'
  *C200874,3 TMI [End  ]   
  RETURN
ENDIF
*C200874,1 NNA (END)
 
*- Printing the new factor name and address format
@ 44,1 SAY 'This Receivable has been assigned to OMNI COMMERCIAL, LLC and has'
@ 45,1 SAY 'Been reassigned and is payable only to:'
@ 46,1 SAY PADC(ALLTRIM(lcFacName),80)
@ 47,1 SAY PADC(ALLTRIM(lcFactAdd),80)
@ 48,1 SAY 'To whom notice must be given of any merchandise returns or claims.'
@ 49,1 SAY 'Payment made to any other party does not constitute valid payment of this invoice.'
*C200820,1 TMI [End  ]  

                  
