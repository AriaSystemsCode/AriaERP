*:****************************************************************
*: Program file  : ARPPRO.PRG
*: Program desc. : Print Pro-Forma Invoice
*: System        : Aria Apparel System (A27).
*: Module        : Account Receivable (AR)
*: Developer     : Khalid Mohi El-Din Mohamed
*: Date          : 04/06/2003
*:****************************************************************
*:C#200514,1
*:B#131650,1  modify Arppro.FRX in group2 change the group expression to  cOrdType +Order+'A'+store+style+STR(Lineno,6)
*:****************************************************************

llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)
lcCurrPost = "LEFT "                              && Default Value.
IF llMulCurr
  IF !USED('SYCINT')
    =gfOpenFile(gcsyshome + "SYCINT" , "CCONTCODE" , 'SH')
  ENDIF
  =SEEK(SycComp.cCont_Code,'SYCINT')
  lcCurrPost = SycInt.cCurrency
ENDIF

*-- Variable to check whether to print the log in the FRX or not.
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)
*-- Variables to get the length of major and non major segment
lnMajor  = LEN(gfItemMask('PM'))
lnNMajor = LEN(gfItemMask('PN'))

*-- Get the tax reference
lcTaxRef = gfGetMemVar('M_TAX_REFE')

*-- Variables to hold the name of the files according to the options.
STORE '' TO lcCustFile, lcOrdFile, lcPkTkFile, lcOrdLine

*-- Logical variables to check if the users has selected customer, SO or PKTKT.
STORE .F. TO llUseCust, llUseOrd, llUsePkTk, llNoRec

*-- Variables to hold the filter for each option
STORE ".T." TO  lcPkTkFltr, lcCustFltr

*-- To check whether to print the report from this program or from the optional one
llarpinv = .T.

*-- Initialize the temporary file
lcOrdHdr  = gfTempName()
lcOrdLine = gfTempName()

*-- Variable to hold the Sold to name, Ship to name, Ship Via Description, Terms Description
*-- Division long name, company phone, company fax
STORE '' TO lcSolTName, lcShpTName, lcShipVia, lcTerms, lcDivLName, lcFacName, lcCompPhon, lcCompFax, lcCompName

*-- Flag to know if we are at the end of the Group
llEndGroup = .F.

*-- Arrays to hold the company, sold to, ship to and division long name
DECLARE laCompAdd[6,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2], laTRltFld[6,2],;
        laEngStyTax[1,2]
STORE '' TO laCompAdd, laSoldTo, laShipTo, laTRltFld, laEngStyTax
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'


*-- Payment term discount, Tax Rate at Style level
STORE 0 TO lnTerDiscR, lnTaxRate

*-- Check whether to print the company information or not.
lcPrnComp = IIF(TYPE('llPrntComp') = 'L' , IIF(llPrntComp , 'Y' , 'N') , 'Y')
llPrntComp = lcPrnComp  = "Y"

*-- To get the taxes description
llTax     = (gfGetMemVar('M_TAX') = 'Y')
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
lcTaxMeth = gfGetMemVar('M_TAX_METH')

*-- To check the type of form FRX or @ Say
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)

*-- To get the selected customers if any. 
lnPosition = ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcCustFile = laOGFxFlt[lnPosition,6]
  llUseCust  = IIF(!EMPTY(lcCustFile) .AND. USED(lcCustFile) .AND. RECCOUNT(lcCustFile)>0,.T.,.F.)
  lcCustFltr = lcCustFltr + IIF(llUseCust," AND SEEK(OrdHdr.Account,lcCustFile)",'')
ENDIF

*-- To get the selected Sales Orders if any.
lnPosition = ASCAN(laOGFxFlt,'ORDHDR.ORDER')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcOrdFile  = laOGFxFlt[lnPosition,6]
  llUseOrd   = IIF(!EMPTY(lcOrdFile) .AND. USED(lcOrdFile) .AND. RECCOUNT(lcOrdFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Pickt Tickets if any.
lnPosition = ASCAN(laOGFxFlt,'PIKTKT.PIKTKT')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcPkTkFile = laOGFxFlt[lnPosition,6]
  llUsePkTk  = IIF(!EMPTY(lcPkTkFile) .AND. USED(lcPkTkFile) .AND. RECCOUNT(lcPkTkFile)>0,.T.,.F.)
  lcPkTkFltr = lcPkTkFltr + IIF(llUsePkTk, "AND SEEK(PikTkt,lcPkTkFile)",'')
ENDIF
                           
*-- Get the company information
=lfCompInfo()

*-- Create the temporary file.
=lfCreatTmp()

*-- If the user selects Orders
IF llUseOrd
  =lfSelByOrd()
ELSE
  *-- If the user selects customers
  IF llUseCust
    =lfSelByCus()
  ELSE
    *-- If the user selects picking tickets or nothing
    =lfSelByAll()
  ENDIF
ENDIF

SELECT (lcOrdLine)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*-- Set the required relations
SET RELATION TO cOrdType+Order INTO (lcOrdHdr) ADDITIVE
SET RELATION TO cOrdType+Order INTO OrdHdr ADDITIVE
SET RELATION TO cOrdType+Order INTO OrdLine ADDITIVE
SET RELATION TO IIF(EMPTY(Store), 'M' + Account , 'S'+Account+Store) INTO CUSTOMER ADDITIVE
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

*-- Get the existance of the form code
lnFrmPos=AT("ARPPRO",lcFormName)+6 
IF !lfGetPath()
  * Message "This form does not exist.Please check your company information settings."
  =gfModalGen('TRM40170B00000','DIALOG','This form')          
  RETURN
ENDIF 

=lfOptProg()

IF llIsAparel
  IF FILE(lcPrgName +'.FXP')
    DO EVAL('lcPrgName')
  ELSE
    * Message "Form 'XX' does not exist.Please check your company information settings."   
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcPrgName,lnFrmPos) + "'" )            
    RETURN
  ENDIF
  
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  lcSavPAdv = _PADVANCE
  IF !lfGetFrx()
    * Message "Form 'XX' does not exist.Please check your company information settings." 
    =gfModalGen('TRM40170B00000','DIALOG',"Form '" +SUBSTR(lcFormName,lnFrmPos) + "'" )          
    RETURN
  ENDIF

  IF llarpinv
    DO gfDispRe WITH EVAL('lcFormName')
  ENDIF

  _PADVANCE = lcSavPAdv
  
ENDIF  

*!**************************************************************************
*! Name      : lfCompInfo
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To get the company information
*!**************************************************************************
*! Example   : =lfCompInfo()
*!**************************************************************************
FUNCTION lfCompInfo

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName   = cCom_Name             && Variable to hold the Company Name
lcCompPhon   = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict   = gfPhoneTem()          && Variable to hold the Company Phone Format
lcCompFax    = cCom_Fax               && Variable to hold the Company Fax
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
lcCompPhon    = TRANSFORM(lcCompPhon , lcPhonPict)
lcCompFax     = TRANSFORM(lcCompFax , lcPhonPict)  && Fax No. Pic
=lfAdrShift('laCompAdd')

*!**************************************************************************
*! Name      : lfCreatTmp
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To create the temporary file
*!**************************************************************************
*! Example   : =lfCreatTmp()
*!**************************************************************************
FUNCTION lfCreatTmp

SELECT OrdHdr

lnFStru = AFIELDS(laFStru)
DIMENSION laFStru[lnFStru+8,4]
laFStru[lnFStru+1,1] = 'nTrde_Disc'
laFStru[lnFStru+1,2] = 'N'
laFStru[lnFStru+1,3] = 5
laFStru[lnFStru+1,4] = 2

laFStru[lnFStru+2,1] = 'Tax_Rate'
laFStru[lnFStru+2,2] = 'N'
laFStru[lnFStru+2,3] = 5
laFStru[lnFStru+2,4] = 2

laFStru[lnFStru+3,1] = 'nShipAmt'
laFStru[lnFStru+3,2] = 'N'
laFStru[lnFStru+3,3] = 14
laFStru[lnFStru+3,4] = 2

laFStru[lnFStru+4,1] = 'Discount'
laFStru[lnFStru+4,2] = 'N'
laFStru[lnFStru+4,3] = 13
laFStru[lnFStru+4,4] = 2

laFStru[lnFStru+5,1] = 'nMerchTax'
laFStru[lnFStru+5,2] = 'N'
laFStru[lnFStru+5,3] = 13
laFStru[lnFStru+5,4] = 4

laFStru[lnFStru+6,1] = 'Tax_Amt'
laFStru[lnFStru+6,2] = 'N'
laFStru[lnFStru+6,3] = 13
laFStru[lnFStru+6,4] = 2

laFStru[lnFStru+7,1] = 'nTotalChg'
laFStru[lnFStru+7,2] = 'N'
laFStru[lnFStru+7,3] = 14
laFStru[lnFStru+7,4] = 2

laFStru[lnFStru+8,1] = 'nTotQty'
laFStru[lnFStru+8,2] = 'N'
laFStru[lnFStru+8,3] = 7
laFStru[lnFStru+8,4] = 0

=gfCrtTmp(lcOrdHdr,@laFStru,'cOrdType+Order+Store',lcOrdHdr)

SELECT OrdLine

=AFIELDS(laFStru)
DIMENSION laFStru[ALEN(laFStru,1)+1,4]
laFStru[ALEN(laFStru,1),1] = 'cType'
laFStru[ALEN(laFStru,1),2] = 'C'
laFStru[ALEN(laFStru,1),3] = 1
laFStru[ALEN(laFStru,1),4] = 0

=gfCrtTmp(lcOrdLine,@laFStru,'cOrdType+Order+cType+Store+Style+STR(LineNo,6)',lcOrdLine)

*!**************************************************************************
*! Name      : lfSelByOrd
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : If the user selects orders from option grid
*!**************************************************************************
*! Example   : =lfSelByOrd()
*!**************************************************************************
FUNCTION lfSelByOrd

SELECT (lcOrdFile)
LOCATE
SCAN
  WAIT WINDOW "Order #: " + Order NOWAIT
  lcOrder = Order

  *-- Seek in the ordhdr file and check if the customer is valid in case of selecting 
  *-- customers in the option grid
  IF !SEEK('O'+lcOrder,'OrdHdr') OR !EVALUATE(lcCustFltr)
    LOOP
  ENDIF
  
  *-- Add record to the header file
  =lfAddHdr()
  
  IF SEEK(OrdHdr.cOrdType+OrdHdr.Order,'OrdLine')
    SELECT OrdLine
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType+OrdHdr.Order
      SCATTER MEMVAR MEMO
      m.ctype = "A"
      lcStore = Store
      INSERT INTO (lcOrdLine) FROM MEMVAR
      =lfReplOHdr()
    ENDSCAN
    
    *-- Add the SO note pad if any
    =lfAddNotPd()

  ENDIF    

ENDSCAN

*!**************************************************************************
*! Name      : lfSelByCus
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : If the user selects customers from option grid
*!**************************************************************************
*! Example   : =lfSelByCus()
*!**************************************************************************
FUNCTION lfSelByCus
PRIVATE lcOrdIndx

lcOrdIndx = ORDER('ORDHDR')
SET ORDER TO OrdAcct IN OrdHdr

SELECT (lcCustFile)
LOCATE
SCAN
  WAIT WINDOW "Account #: " + Account NOWAIT
  lcAcct = Account
  IF SEEK(lcAcct,'OrdHdr')
    llAddRec = .F.
    SELECT OrdHdr
    SCAN REST WHILE Account+cOrdType+Order = lcAcct FOR cOrdType = "O" AND Status $ 'O|H'
      WAIT WINDOW "Order #: " + Order NOWAIT
      lcOrder = Order
       
      *-- Add record to the header file
      =lfAddHdr()

      *-- To add the lines
      =lfAddLines()
      
    ENDSCAN
  ENDIF
ENDSCAN

SET ORDER TO TAG &lcOrdIndx IN OrdHdr

*!**************************************************************************
*! Name      : lfSelByAll
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : If the user select by pickting tickets of nothing
*!**************************************************************************
*! Example   : =lfSelByAll()
*!**************************************************************************
FUNCTION lfSelByAll

SELECT OrdHdr
SEEK 'O'
SCAN REST WHILE cOrdType+Order = 'O' FOR Status $ 'O|H'
  WAIT WINDOW "Order #: " + Order NOWAIT
  lcOrder  = Order
  llAddRec = .F.

  *-- Add record to the header file
  =lfAddHdr()
  
  *-- To add the SO lines.
  =lfAddLines()

ENDSCAN

*!**************************************************************************
*! Name      : lfAddHdr
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To add a record to the header file and get the term payment discount
*!**************************************************************************
*! Example   : =lfAddHdr()
*!**************************************************************************
FUNCTION lfAddHdr
PRIVATE lnAlias

DECLARE laTRltFld[6,2]
laTRltFld = ''
lnAlias = SELECT(0)

SELECT OrdHdr
SCATTER MEMVAR MEMO
INSERT INTO (lcOrdHdr) FROM MEMVAR
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnTerDiscR'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnTDaysDue'
laTRltFld[4,1] = 'CODYN'
laTRltFld[4,2] = 'lcTCod'
laTRltFld[5,1] = 'LINSTALLM'
laTRltFld[5,2] = 'llInstTerm'
laTRltFld[6,1] = 'EOMDAY'
laTRltFld[6,2] = 'lnEomDay'

=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')

=SEEK(IIF(EMPTY(Store),'M'+OrdHdr.Account,'S'+OrdHdr.Account+OrdHdr.Store),'Customer')

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfAddLines
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To add the SO lines
*!**************************************************************************
*! Example   : =lfAddLines()
*!**************************************************************************
FUNCTION lfAddLines

IF SEEK(OrdHdr.cOrdType+OrdHdr.Order,'OrdLine')
  SELECT OrdLine
  SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = OrdHdr.cOrdType+OrdHdr.Order
    IF !EVALUATE(lcPkTkFltr) OR (lcRpSellBy = "P" AND TotPik = 0)
      LOOP
    ENDIF
    llAddRec = .T.
    SCATTER MEMVAR MEMO
    m.ctype = "A"
    lcStore = Store
          
    INSERT INTO (lcOrdLine) FROM MEMVAR
    *-- If select by picking ticket then replace the Qty&n WITH PIK&n to do not have
    *-- to use two different fields
    IF lcRpSellBy = "P"
      SELECT (lcOrdLine)
      FOR lnCnt = 1 TO 8
        lcCnt = STR(lnCnt,1)
        REPLACE Qty&lcCnt WITH Pik&lcCnt
      ENDFOR
      REPLACE TotQty WITH TotPik
    ENDIF

    =lfReplOHdr()

  ENDSCAN
ENDIF  

*-- To add the SO note pad if any 
IF llAddRec
  =lfAddNotPd()
ENDIF  

*!**************************************************************************
*! Name      : lfReplOHdr
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To calculate the tax rate, tax amount and total amount
*!**************************************************************************
*! Example   : =lfReplOHdr()
*!**************************************************************************
FUNCTION lfReplOHdr
PRIVATE lnAlias

lnAlias = SELECT(0)
=SEEK(Style,'Style')
laEngStyTax[1,1] = 'NTAXRATE'
laEngStyTax[1,2] = 'lnTaxRate'
STORE 0 TO lnTaxQty,lnTaxRate
IF !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
  =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE')
  FOR lnCount = Style.nTaxBreak TO 8
    lnTaxQty = lnTaxQty + EVALUATE(lcOrdLine+'.Qty'+STR(lncount,1))
  ENDFOR
ENDIF


SELECT (lcOrdHdr)
IF SEEK (OrdHdr.cOrdType+OrdHdr.Order+OrdHdr.Store)
  REPLACE nTrde_Disc WITH lnTerDiscR                                           ,;
          Tax_Rate   WITH Customer.nTaxRate                                    ,;
          nShipAmt   WITH nShipAmt  + &lcOrdLine..TotQty * &lcOrdLine..Price   ,;
          Discount   WITH -nShipAmt * OrdHdr.Disc/100                          ,;
          nMerchTax  WITH nMerchTax + lnTaxQty*&lcOrdLine..Price*lnTaxRate/100 ,;
          Tax_Amt    WITH nMerchTax*(100-OrdHdr.Disc)/100*(100-nTrde_Disc)/100 ,;
          nTotalChg  WITH nShipAmt+Tax_Amt+Discount                            ,;
          nTotQty    WITH nTotQty + &lcOrdLine..TotQty
ENDIF

SELECT(lnAlias)

*!**************************************************************************
*! Name      : lfAddNotPd
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : To add the SO notepad
*!**************************************************************************
*! Example   : =lfAddNotPd()
*!**************************************************************************
FUNCTION lfAddNotPd

IF SEEK('B'+lcOrder,'NotePad')
SELECT (lcOrdLine)
APPEND BLANK
REPLACE cType    WITH 'B'            ,;
        cOrdType WITH 'O'            ,;
        Order    WITH lcOrder        ,;
        Account  WITH OrdHdr.Account ,;
        Store    WITH lcStore        ,;
        Note_Mem WITH NotePad.mNotes
ENDIF


*!**************************************************************************
*! Name      : lfSolSpAdr
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via & Terms. Called from FRX
*!**************************************************************************
*! Example   : =lfSolSpAdr()
*!**************************************************************************
FUNCTION lfSolSpAdr

llEndGroup = .F.
=gfRltFld(OrdHdr.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(OrdHdr.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(OrdHdr.cTermCode , 'CTERMCODE')

lcSolTName = Customer.BTName

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

  lcShpTName  = IIF(EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF

=lfAdrShift('laShipTo')

RETURN ''

*!**************************************************************************
*! Name      : lfvSelBy
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 04/06/2003
*! Purpose   : Valid function for select by option
*!**************************************************************************
*! Example   : =lfvSelBy()
*!**************************************************************************
FUNCTION lfvSelBy
CLEAR READ

*!*************************************************************
*! Name      : lfGetPath						
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Check If the file exist or not..
*!*************************************************************
*! Example	      	 : =lfGetPath
*!*************************************************************
FUNCTION lfGetPath

IF !EMPTY(lcOptProg)
  lcPath = ALLTRIM(lcOptProg)
  lcPath = IIF(FILE(gcRepHome+lcPath+'.FXP'),;
                   gcRepHome+lcPath,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcPath+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcPath,;
  				            gcRepHome+LEFT(lcPath,2)+'\'+lcPath)) 
   
  RETURN FILE(ALLTRIM(lcpath) +'.FXP')  
ENDIF  

*!*************************************************************
*! Name      : lfGetFrx						
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Check If the file exist or not.
*!*************************************************************
*! Example	      	 : =lfGetFrx
*!*************************************************************
FUNCTION lfGetFrx

IF RAT('\',lcFormName)=0
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
  				     .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
  				     gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
ENDIF  				   
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPPROA.PRG , lfSolSpAdr()
*!*************************************************************
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the INVHDR file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ARPPROA.FRX
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.

RETURN ''

*!**************************************************************************
*! Name      : lfCurrDesc
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Get the currency description
*!**************************************************************************
*! Example   : = lfCurrDesc()
*!**************************************************************************
FUNCTION lfCurrDesc
PRIVATE lcCurrDsc

lcCurrDsc = ''
lcCurrDsc = IIF(SEEK(OrdHdr.CCURRCODE,'SYCCURR'),SYCCURR.cCurrDesc,lcCurrDsc)

RETURN ALLTRIM(lcCurrDsc)

*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Khalid Mohi El-Din (KHM)
*! Date      : 04/06/2003
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(OrdHdr.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign
