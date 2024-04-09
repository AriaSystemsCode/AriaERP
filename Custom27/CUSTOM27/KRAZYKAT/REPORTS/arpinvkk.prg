*:***************************************************************************
*: Program file  : ARPINVKK
*: Program desc. : Print Custom Invoice Form for (Krazy Kat) (REFERE TO #C101646)
*: Date          : 10/06/1999
*: System        : Aria Advantage Series.
*: Module        : ACCOUNTS RECEIVABLE (AR)
*: Developer     : Sameh Saiid (SSE)
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM for Krazy Kat (101646) 
*:**************************************************************************
*: Functions : lfNonMaj() , lfPrintSku() , lfSetSkip()
*:**************************************************************************
*: Modifications :
*: B804070,1 BWA 04/26/2001 Print the orginal store add. instead of the DC address.
*: B605981,1 SSE 07/17/2002 Fix bug of not printing Total pcs & Cartons.
*: B605981,1                Modifications is in FRX.
*: B123495,1 BWA 07/22/2004 Comment the red lines in the report.
***************************************************************************

*B804070,1 BWA 04/26/2001 Print the orginal store add. instead of the DC address.[START]
STORE '' TO lcShipVKK , lcTermKK
*B804070,1 [END]

DIMENSION laClrlName[1,2]
laClrlName[1,1] = 'CLRLNAME'
laClrlName[1,2] = 'lcClrlName'
lnrpAls = SELECT(0)

*-- We need to reopren Spck_Lin to get the last record number in a variable. 
=gfOpenFile(gcDataDir+"Spck_Lin","SpckLins",'SH', @lcDumFile,.T.)

SELECT InvHdr
SET SKIP TO InvLIne,Spck_Lin

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C'
    lnClrPo    = laMajSeg[lnI,4]
    lcNonMajPi = laMajSeg[lnI,3]
    EXIT  
  ENDIF
ENDFOR    && end Loop Around Non Major elements.

lnColorLen = LEN(lcNonMajPi)
*-- Compute Free/Color Items in Style Structure. [End]

SELECT (lnrpAls)
****************************************************************************
********************** *-- Functions used in FRX --* ***********************
****************************************************************************

*!**************************************************************************
*! Name      : lfPrintSku
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/06/1999
*! Purpose   : print the SKU if its Quantity in Invoice file is not zero
*!**************************************************************************
*! Example   : lfPrintSku()
*!**************************************************************************
*
FUNCTION lfPrintSku
PARAMETER llRetVal
PRIVATE lnCounter
lnCounter= 1
llRetVal = .F.
FOR lnCounter = 1 TO 8
  IF EVAL('Spck_lin.Qty'+STR(lnCounter,1))=1
    llRetVal = EVAL('INVLINE.Qty'+STR(lnCounter,1)) > 0
    EXIT
  ENDIF 
ENDFOR
*-- End of lfPrintSku.

*!**************************************************************************
*! Name      : lfEndOfSKU
*! Developer : Sameh Saiid (SSE)
*! Date      : 10/06/1999
*! Purpose   : To get the Record count of the Spck_Lin.DBF
*!**************************************************************************
*! Called from : ARPINVKK.FRX
*!**************************************************************************
*
FUNCTION lfEndOfSKU
PARAMETER lcDummy
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcDumFile)
SET ORDER TO TAG SpckLins DESCENDING

=SEEK('S'+&lcInvLine_A..Account+&lcInvLine_A..Style)
lnSkuRecNo = IIF(EOF(lcDumFile),0,RECNO(lcDumFile))

SET ORDER TO TAG SpckLins ASCENDING
SELECT (lcAlias)
*-- End of lfEndOfSKU.

*B804070,1 BWA 04/26/2001 Print the orginal store add. instead of the DC address.[START]
*!*************************************************************
*! Name      : lfSolSKK
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/26/2001
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       :
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolSKK
PARAMETER lcSolShp

PRIVATE lcAlias , lcInvHdr
STORE '' TO lcShipVKK , lcTermKK

lcInvHdr = SELECT(0)
SELECT INVHDR

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laFactor[5,1] , laDivLName[1,2]

laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
laSoldTo = ''                     && Array to hold the Sold To address
laShipTo = ''                     && Array to hold the Ship To address
laFactor = ''

IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF OrdHdr.Alt_ShpTo
  lcShpTName  = OrdHdr.STName
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE
  lcAlias = SELECT(0)
  SELECT CUSTOMER

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')

=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVKK = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTermKK = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

SELECT(lcInvHdr)
RETURN ''

*-- End of lfSolSKK.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/26/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVKK.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*B804070,1 [END]