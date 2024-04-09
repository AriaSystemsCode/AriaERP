*:***************************************************************************
*: Program file  : ARPINVBN.PRG (Custom Invoice for for Bong)
*: Program desc. : Invoice Form
*: Date          : 03/07/2007
*: System        : Aria 4XP.
*: Module        : Account Receivable (AR)
*: Developer     : Ayman Mahmoud Ahmed (AYM)
*: Ticket No     : T20070206.0009
*: Tracking Job  : C200764 
*:***************************************************************************
*: Example : DO ARPINVBN.PRG
*: C200815,1 MMT 07/10/2007 print the factor information at the end of report[T20070613.0006]
*: B608467,1 SSH Incorrect last page
*: C200984,1 NNA 04/11/2008 (T20070927.0008) Display a new Invoice form message if The factor name is CAPITAL FACTORS, INC. 
*: C201038,1 MMT 08/05/2008 Print HANA Factor address if invoice is using it.[T20080630.0012]
*:***************************************************************************
IF llOGFltCh
  llDonprnt=.F.
	STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst

	lcTmpMain=loOgScroll.gfTempName()
	lcTmpLines=loOgScroll.gfTempName()
	lcDumFile=loOgScroll.gfTempName()
	=lfBuildTmp()

	SELECT (lcTmpMain)
	INDEX on INVOICE TAG (lcTmpMain)

	SELECT (lcTmpLines)
	INDEX on INVOICE+line_NO TAG (lcTmpLines)


	lnTotLinDisc = 0
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
	
	COPY ALL FOR &lcRpExp TO (oAriaApplication.WorkDir+lcDumFile)

	IF !USED(lcDumFile)
	  *-- there is record in SYREPUVR file for gfTempName called lcDumFile
	  *-- this will be used to carry the INVOICE HEADER TEMP. File
	  = gfOpenFile(oAriaApplication.WorkDir+lcDumFile,' ','EX')
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
	  llDonprnt=.T.
	  = gfModalGen('TRM00052B00000','DIALOG' )
	  RETURN

	ENDIF

	=lfCollectdata()

	

	SELECT (lcTmpMain)
	IF !RECCOUNT()>0
	  llDonprnt=.T.
	  *-- Message : There are no records to display...!
	  *--                < Ok > 
	    =gfModalGen('TRM00052B40011','ALERT')
	  RETURN
	ENDIF 

	=lfAdjustCRSettings()
	IF USED(lcTmpMain)
	    USE IN (lcTmpMain)
	ENDIF

	IF USED(lcTmpLines)
	    USE IN (lcTmpLines)
	ENDIF

	=gfDispRe()

ELSE
  IF llDonprnt
    *-- Message : There are no records to display...!
    *--                < Ok > 
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN
  ELSE
    =gfDispRe()
  ENDIF  
ENDIF  &&FILTER CHANGE
*!**************************************************************************
*! Name      : lfFillMain
*: Developer : AYMAN MAHMOUD AHMED
*! Date      : 03/07/2007
*! Purpose   : To FILL MAIN TEMP FILE
*!**************************************************************************
*! Example   :  =lfPrintHdr()
*!**************************************************************************
FUNCTION lfFillMain
PRIVATE lnAlias

lnAlias = SELECT(0)
=lfCustInfo()

M.INVOICE=InvHdr.Invoice
M.ACCOUNT= CUSTOMER.Account


IF !EMPTY(INVHDR.STORE)
	M.STNAME=INVHDR.STORE
	M.BTNAME= lcBtName
ELSE	
	M.BTNAME= lcBtName
	M.STNAME= lcStName
ENDIF

M.BTADD1=lcBtAdd1
M.BTADD2= IIF(!EMPTY(lcBtAdd2),lcBtAdd2,lcBtAdd3)
M.BTADD3= IIF(!EMPTY(lcBtAdd2),lcBtAdd3,'')



M.STADD1=lcStAdd1
M.STADD2=IIF(!EMPTY(lcStAdd2),lcStAdd2,lcStAdd3)
M.STADD3=IIF(!EMPTY(lcStAdd2),lcStAdd3,' ')

M.INVDATE= InvHdr.InvDate

M.SHIPVIA=ALLTRIM(gfCodDes(InvHdr.ShipVia,'SHIPVIA  '))
M.TERMS =ALLTRIM(gfCodDes(InvHdr.cTermCode,'CTERMCODE '))

M.CustPo= ALLTRIM(INVHDR.CustPo)
M.ORD_ENT=ORDHDR.Entered
M.REP1=ALLTRIM(INVHDR.Rep1)
M.ORDER= INVHDR.Order

M.SHIPPED=InvHdr.Ship

M.SHIPAMT=InvHdr.ShipAmt+lnTotLinDisc
m.disc=lnTotLinDisc-InvHdr.Discount

lnTotLinDisc=0
M.Charge=INVHDR.Freight + INVHDR.Insur + INVHDR.COD

M.AMNTPAY=InvHdr.ShipAmt + INVHDR.Freight + INVHDR.Insur + INVHDR.COD + InvHdr.Discount
M.ORDNOTE=""
IF llRpInvNot AND SEEK ('B' + InvHdr.Order,'NotePad')
   M.ORDNOTE= NotePad.mNotes
 ENDIF
M.INVNOTE=""
	IF llRpInvNot AND SEEK ('C' + InvHdr.Invoice,'NotePad')
	  M.INVNOTE= NotePad.mNotes
	ENDIF

  M.cFactor=IIF( !EMPTY(InvHdr.cFacCode),'Y','N')
  *C200815,1 MMT 07/10/2007 print the factor information at the end of report[Start]
  IF !EMPTY(INVHDR.CFACCODE)   
    =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')

    m.cFactName = lcFacName
    m.CFactAdd  = ALLTRIM(laFactor[1])+IIF(!EMPTY(ALLTRIM(laFactor[1])),",","")+;
                  ALLTRIM(laFactor[2])+IIF(!EMPTY(ALLTRIM(laFactor[2])),',',"")+;
                  allTRIM(laFactor[3])+IIF(!empty(allTRIM(laFactor[3])),',',"")+ALLTRIM(laFactor[4])+;
                  IIF(!EMPTY(ALLTRIM(laFactor[4])),',',"")+laFactor[5]
    *C200984,1 NNA 04/11/2008 (BEGIN) update the Temp file(header) with the CPT Factor information
    IF ALLTRIM(INVHDR.CFACCODE)=='CPT'
      m.cFac_Note1= 'This bill and all future bills are assigned to, owned by and are payable only'
      m.cFac_Note2='To: '+lcFacName
      m.cFac_Note3=laFactor[1]
      m.cFac_Note4=laFactor[2]
      m.cFac_Note5='To whom prompt notice must be given of any merchandise returns and any claims or'
      m.cFac_Note6='disputes whether based on shortages, non-delivery, offsets or other claims.'
    ELSE
    
      *: C201038,1 MMT 08/05/2008 Print HANA Factor address if invoice is using it.[Start]
      IF ALLTRIM(INVHDR.CFACCODE)=='HANA'
        m.cFac_Note1 = 'This invoice is assigned to, owned by, and payable only to:'
        m.cFac_Note2 = 'HANA FINANCIAL, INC.'
        m.cFac_Note3 = 'FILE NO. 50516'
        m.cFac_Note4 = 'LOS ANGELES, CA 90074-0516'
        m.cFac_Note5 = 'to whom prompt notice must be given of any objections to payment. Payment made to'
        m.cFac_Note6 = 'any other party does not constitute valid payment.     Telephone: 800-482-4262'
      ELSE
      *: C201038,1 MMT 08/05/2008 Print HANA Factor address if invoice is using it.[End]
      
        m.cFac_Note1 = 'This Receivable has been assigned to OMNI COMMERCIAL, LLC and has'
        m.cFac_Note2 = 'Been reassigned and is payable only to: '
        m.cFac_Note3 = lcFacName
        m.cFac_Note4 = m.CFactAdd
        m.cFac_Note5 = 'To whom notice must be given of any merchandise returns or claims. '
        m.cFac_Note6 = 'Payment made to any other party does not constitute valid payment of this invoice.'
      
      *: C201038,1 MMT 08/05/2008 Print HANA Factor address if invoice is using it.[Start]
      ENDIF 
      *: C201038,1 MMT 08/05/2008 Print HANA Factor address if invoice is using it.[End]
    ENDIF
    *C200984,1 NNA (END)
  ENDIF 
  *C200815,1 MMT 07/10/2007 print the factor information at the end of report[End]
*:B608467,1 SSH If print notes
  m.cPrintMemo = IIF(llRpInvNot AND !EMPTY(M.INVNOTE),'Y','N')
*:B608467,1 SSH 

INSERT INTO (lcTmpMain) FROM  MEMVAR


SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfFillins
*: Developer : AYMAN MAHMOUD AHMED
*! Date      :03/07/2007
*! Purpose   : To FILL the invoice lines
*!**************************************************************************
*! Example   :  =lfPrnLines()
*!**************************************************************************
FUNCTION lfFillins
LPARAMETERS cpInvNo
PRIVATE lnAlias
lnAlias = SELECT(0)

*:B608467,1 SSH use invline instead
*!*	IF InvHdr.CONSOL = 'Y'
*!*	   SELECT CONSINVL
*!*	ELSE
SELECT INVLINE
*!*	ENDIF
*:B608467,1 SSH 

STORE 0 TO lnTotAmt, lnTotShip
*:B608467,1 SSH If print notes
*=SEEK(&lcDumFile..Invoice)
=SEEK(cpInvNo)
*SCAN REST WHILE Invoice+STR(LineNo,6) = &lcDumFile..Invoice
*:B608467,1 SSH If print notes
SCAN REST WHILE Invoice+STR(LineNo,6) = cpInvNo

  WAIT WINDOW NOWAIT 'Style-Color :'+ALLTRIM(STYLE)
  =Seek("O"+INVLINE.Order+Str(INVLINE.LineNo,6),"OrdLine")

  *m.Invoice=InvLine.Invoice
  m.Invoice=INVLINE.Invoice
  *m.line_no=lfGetstr(InvLine.lineno)
  m.line_no=lfGetstr(INVLINE.lineno)
  M.ORDBOOK= OrdLine.TotBook
  M.INVQTY= INVLINE.TotQty
  M.ORDQTY=OrdLine.TotQty 
  
  M.STYLE=LEFT(ALLTRIM(SUBSTR(STYLE,1,lnMajorLen))+ ALLTRIM(SUBSTR(Style,lnNonMajSt,lnColorLen)),10)

  M.COLOR=LEFT(IIF(!EMPTY(STYLE.PREPAK),STYLE.PREPAK+'-','')+ALLTRIM(gfCodDes(ALLTRIM(SUBSTR(Style,lnNonMajSt,lnColorLen)),'COLOR  '))+"-"+ALLTRIM(SUBSTR(OrdLine.DESC1,1,22)),19)
  M.GR_PRICE=INVLINE.GROS_PRICE
  M.PRICE=INVLINE.PRICE
 
  lnTotLinDisc = lnTotLinDisc + ((INVLINE.TOTQTY*INVLINE.GROS_PRICE*INVLINE.DISC_PCNT)/100)
  INSERT INTO (LCTMPLINES) FROM MEMVAR
ENDSCAN


WAIT CLEAR

*:***************************************************************************
*: Name        : lfCollectdata
*: Developer   : Mohamed Shokry(MHM)
*! Date        : 03/08/2007
*: Purpose     : Collect Data For Report
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*! Example     : = lfPrevPrn()
*:***************************************************************************
FUNCTION lfCollectdata

SELECT (lcDumFile)
GO TOP

SCAN REST WHILE INKEY() <> 32
  lcInvoice = InvHdr.Invoice    && store invoice no to be used in updating prtFlag field
  WAIT WINDOW 'PRINTING INVOICE ' + INVOICE + ' - <Space Bar> TO ABORT' NOWAIT
*:B608467,1 SSH Send invoice number as parameter
  =lfFillins(lcInvoice)
  =lfFillMain()

*!*	*khalid
	IF oAriaApplication.gcDevice = 'PRINTER' AND SEEK(lcInvoice,'InvHdr')
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



*!*	*!**************************************************************************
*!*	*! Name      : lfPrnFac
*!*	*! Developer : ASH - Ashraf Sherif Mohammed
*!*	*! Date      : 02/11/2004
*!*	*! Purpose   : To print factor information
*!*	*! B121578,1 
*!*	*!**************************************************************************
*!*	*! Example   :  =lfPrnFac()
*!*	*!**************************************************************************
*!*	FUNCTION lfPrnFac
*!*	@ 44,1 SAY 'THIS ACCOUNT AND THE MERCHANDISE IT REPRESENTS HAS BEEN ASSIGNED TO AND IS'
*!*	@ 45,1 SAY 'PAYABLE ONLY TO ROSENTHAL & ROSENTHAL, INC. P.O.BOX 88926, CHICAGO, IL '
*!*	@ 46,1 SAY '60695-1926 MAKE CHECK PAYABLE AS ABOVE IN UNITED STATES FUNDS AND INDICATE'
*!*	@ 47,1 SAY 'NAME OF SELLER. ANY CLAIMS AGAINST THIS INVOICE MUST BE MADE PROMPTLY IN '
*!*	@ 48,1 SAY 'WRITING TO ROSENTHAL & ROSENTHAL,INC. ,SPECIFYING DETAILS. NO RETURNS OR'
*!*	@ 49,1 SAY 'ADJUSTMENTS WILL BE RECOGNIZED WITHOUT THE WRITTEN CONSENT OF ROSENTHAL &'
*!*	@ 50,1 SAY 'ROSENTHAL, INC.'


*************************************************************
*! Name      : lfBuildTmp
*! Developer : AYMAN MAHMOUD AHMED (AYM)
*! Date      : 05/30/2006
*! Purpose   : 
*!*************************************************************
FUNCTION lfBuildTmp

*C200815,1 MMT 07/10/2007 print the factor information at the end of report[Start]
*DIMENSION laTempStru[25,18]
*:B608467,1 SSH Add field for Notes
*DIMENSION laTempStru[27,18]

*C200984,1 NNA 04/11/2008 (BEGIN) Change the Array to add more fields
*DIMENSION laTempStru[28,18]
DIMENSION laTempStru[34,18]
*C200984,1 NNA (END)
*:B608467,1 SSH
*C200815,1 MMT 07/10/2007 print the factor information at the end of report[End]

STORE '' TO laTempStru
STORE 0 TO lnIndex

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'INVOICE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDER'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CUSTPO'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORD_ENT'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ACCOUNT'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SHIPVIA'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'TERMS'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTNAME'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*10
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'BTADD3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STNAME' 
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STADD3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0
*15
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'INVDATE'
laTempStru[lnIndex,2] = 'D'
laTempStru[lnIndex,3] = 8
laTempStru[lnIndex,4] = 0


lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'REP1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 3
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SHIPPED'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'SHIPAMT'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CHARGE'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'AMNTPAY'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDNOTE'
laTempStru[lnIndex,2] = 'M'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'INVNOTE'
laTempStru[lnIndex,2] = 'M'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CFACTOR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'DISC'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 10
laTempStru[lnIndex,4] = 2


*C200815,1 MMT 07/10/2007 print the factor information at the end of report[Start]
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFactName'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'CFactAdd'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0
*C200815,1 MMT 07/10/2007 print the factor information at the end of report[End]
*:B608467,1 SSH Add field for Notes
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cPrintMemo'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 1
laTempStru[lnIndex,4] = 0

*:B608467,1 SSH Add field for Notes
*C200984,1 NNA 04/11/2008 (BEGIN) add the new fields that I'll use to display factor information on the report
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note1'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note2'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note3'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note4'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note5'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'cFac_Note6'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 100
laTempStru[lnIndex,4] = 0
*C200984,1 NNA (END)

*Create Temp*
=gfCrtTmp(lcTmpMain,@laTempstru,,"",.f.)
DIMENSION laTempStru[9,18]
STORE '' TO laTempStru
STORE 0 TO lnIndex
lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'INVOICE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'STYLE'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 19
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'COLOR'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 30
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'LINE_NO'
laTempStru[lnIndex,2] = 'C'
laTempStru[lnIndex,3] = 4
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDBOOK'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0


lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'ORDQTY'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'INVQTY'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 0

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'GR_PRICE'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 2

lnIndex=lnIndex+1
laTempStru[lnIndex,1] = 'PRICE'
laTempStru[lnIndex,2] = 'N'
laTempStru[lnIndex,3] = 6
laTempStru[lnIndex,4] = 2

*Create Temp*
=gfCrtTmp(lcTmpLines,@laTempstru,,"",.f.)


*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 06/26/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[2]

*DIMENSION loOgScroll.laCRParams[1,2]
DIMENSION loOgScroll.laCRParams[2,2]

loOgScroll.lcOGLastForm ='ARPINVBN'
loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcTmpMain+ ".DBF"
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcTmpLines+ ".DBF"

  
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2]= ' '

loOgScroll.laCRParams[2,1] = 'cPrintMemo'
loOgScroll.laCRParams[2,2] = IIF(llRpInvNot,'Y','N')


*************************************************************
*! Name      : lfGetstr
*! Developer : AYMAN MAHMOUD AHMED (SMM)
*! Date      : 03/07/2007
*! Purpose   : Convert line no to string
*!*************************************************************
FUNCTION lfGetstr
PARAMETERS Nlineno

PRIVATE Cret

Cret=RIGHT('000'+ALLTRIM(STR(Nlineno)),4)

RETURN Cret