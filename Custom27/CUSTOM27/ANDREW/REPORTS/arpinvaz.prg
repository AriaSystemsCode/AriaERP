*:**************************************************************************
*: PROGRAM   : ARPINVAZ.PRG                FOR : AND100
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (C101385)
*:**************************************************************************
*: Calls : 
*:         FUNCTION  : lfPriHdr()
*:                   : lfLoadVar()
*:                   : lfClear()
*:					 : lfGetWareh()
*:					 : lfPriBod()
*:					 : lfColctLin()
*:					 : lfPriScl()
*:					 : lfPrnStrn()
*:					 : lfGetLoc() 
*:					 : lfContuLoc()
*:					 : lfPrnNotPd()
*:					 : lfPrnFot()
*:         PROCEDURE : lpPrtSku()
*:**************************************************************************
*: Modifications:
*: B603400,1 ADEL 01/25/2000  1- Print the number of cartons and the weight by adding 1 line *: *: between the TOTAL MERCHANDISE & the FREIGHT.
*: B603400,1                  2- Change the Factor name to:
*: B603400,1                  3- Print the GST TAX incase of Canada or the regular
*: B603400,1                     tax incase of USA above the invoice amount.
*: B603400,1                  4- Print the BOL # before the TOTAL MERCHANDISE line. 
*: B803007,1 Suppress the empty lines in Comp. Address
*: B803123,1 SHA 03/16/2000 Added divisions (C2, B2 and G2) to the list of the divisions
*: B803123,1 SHA            that show Duns# 84-895-9474
*: B803123,1 SHA            Added divisions (C1, B1 and G1) to the list of the divisions
*: B803123,1 SHA            that show Duns# 62-374-5114
*:**************************************************************************

*C101385 (Begin) If No records selected.
*AHM Save Remit to variables to the MEM file (Start)
lnCart     = 0
lnWeig     = 0
lcBIllOfL  = ''
*C101672,1 (Begin) Initialize needed variables for division long name.
*DECLARE laDisRltFld[1,1]
DECLARE laDisRltFld[1,2]
laDisRltFld[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDisRltFld[1,2] = 'lcDivLName'
*C101672,1 (End)
*AHM Save Remit to variables to the MEM file (End)

lnMaxRow = 52
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*C101385 (End)
*--Get the warehouse flag.
llMultiWH  = (gfGetMemVar('M_WareHouse') = 'Y')
STORE  1  TO lnCurObj 
*:AHM Get Taxes (Start)
lcTaxDesc = gfGetMemVar('M_TAX_DESC')
llIsCanada = IIF (UPPER(ALLTRIM(gcContCode))='CANADA',.T.,.F.)
*:AHM Get Taxes (Start)


*C101672,1 (Begin) Get the 'Remit to' addresss.
SET DEVICE TO SCREEN
STORE SPACE(70) TO lcNote1,lcNote2,lcNote3,lcNote4,lcNote5  
IF FILE(gcDataDir+"ARPINVAZ.MEM")
  RESTORE FROM (gcDataDir+"ARPINVAZ.MEM") ADDI
ENDIF
DO (gcRepHome +"ARPINVAZ.SPX")

*STORE 0 TO lnPbOk
*DEFINE WINDOW NOTE FROM 1,1 to 9,74 SHADOW COLOR SCHEME 6 TITLE "Remit To"
*MOVE WINDOW NOTE CENTER
*ACTIVATE WINDOW NOTE
*DO WHILE lnPbOk <> 1
*  @ 1,1 GET lcNote1 
*  @ 2,1 GET lcNote2 
 * @ 3,1 GET lcNote3 
*  @ 4,1 GET lcNote4 
*  @ 5,1 GET lcNote5
*  @ 6,30 GET lnPbOk  DEFAULT 1 FUNCTION '*  \!\<OK' SIZE 1,10 COLOR SCHEME 5
*  READ
*ENDDO
SAVE ALL LIKE lcNote*.* TO (gcDataDir+"ARPINVAZ.MEM")
RELEASE WINDOW NOTE
*SET DEVICE TO PRINT
*C101672,1 (End)
*-- Opening necessary files
=gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
=gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
=gfOpenFile(gcDataDir+'WhsLoc',gcDataDir+'WhsLocSt','SH')

*AHM Open Pack_Hdr file (Start)
=gfOpenFile(gcDataDir+'pack_hdr','Orderpck','SH')
*AHM Open Pack_Hdr file (End)

lcTmpFile = gfTempName()
*C101385 (Begin) Get the company addrress.
*--Fill the company address variables.

*B803007,1 Suppress the empty lines in Comp. Address (Start)
*lcComAdr2 = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
*lcComAdr3 = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
*lcComAdr4 = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)

lcComAdr1 = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
lcComAdr2 = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
lcComAdr3 = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
lcComAdr4 = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
lcComAdr5 = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
PRIVATE laComAdd
DIMENSION laComAdd[5]
laComAdd[1] = lcComAdr1
laComAdd[2] = lcComAdr2
laComAdd[3] = lcComAdr3
laComAdd[4] = lcComAdr4
laComAdd[5] = lcComAdr5
=lfAdrShift('laComAdd')
lcComAdr1 = laComAdd[1] 
lcComAdr2 = laComAdd[2]
lcComAdr3 = laComAdd[3]
lcComAdr4 = laComAdd[4]
lcComAdr5 = laComAdd[5]
*B803007,1 Suppress the empty lines in Comp. Address (End)

*C101385 (End)
*C101385 (Begin) Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
*--Get the color
STORE 0 TO lnColorLen,lnNonMajSt
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
*--- Variable declaration
lcSkuSize  = ' '
DIMENSION laAddress[1,1]
laAddress = ''
lcStrToPrn = ' '
lnLen      = 0
lcStr      = ' '
lnAmnt     = 0
lcScale    = SPACE(01)
lcStyColD  = SPACE(30)
lcInvoice  = SPACE(06)
lcAccount  = SPACE(05)
lcWareCode = SPACE(06)
lcWCity    = SPACE(15)
lcWState   = SPACE(03)
ldInvDate  = {}
*--Initialized the ROW variable in order to move the entire header four lines down.
ROW        = 4
lnMerch    = 0
lnTotChg   = 0
lnShip     = 0
lnFrei     = 0
lnDisc     = 0
lnMerch    = 0
lnShip     = 0
lcDuns     = SPACE(11)
lcSalesNam = SPACE(25) 
lcStore    = SPACE(7)
lcRep1     = SPACE(03)
lcDuns     = SPACE(11)
lcOrder    = SPACE(06)
lcNotes1   = SPACE(30)
lcNotes2   = SPACE(30)
lcDivLName = SPACE(30)
lcApproval = SPACE(10)
lcPikTkt   = SPACE(06)
lcCustPo   = SPACE(10)
lcDept     = SPACE(05)
lcShiVCode = SPACE(15)
lcTerVCode = SPACE(15)
lcColor    = SPACE(06)
lcStyle    = SPACE(12)
lcFactor   = SPACE(06)
lnPageNo   = 0
*-- Bill to AND *- Ship to Addresses.
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5
*-- End Variable declaration
SELECT INVHDR
llFirst = .T.
*C101385 (Begin)  Browse the lineup message.
llLineUp  = gfModalGen('QRM40145B40012','DIALOG' ) = 1
*C101385 (End)
SET DEVICE TO PRINT
*C101385 We will make SCAN statement within a DO WHILE statement to care the line up.
DO WHILE .T.
  SCAN WHILE INKEY() <> 32 FOR &lcRpExp
    =lfClear()
    =lfLoadVar()
    =lfColctLin()
    =lfPriHdr()
    =lfPriBod()
    =lfPrnFot()
    IF llLineUp AND llFirst
      SELECT INVHDR
      IF !BOF()
        SKIP-1
      ENDIF  
      llFirst = .F.
      EXIT
    ENDIF
    IF llLineUp
      *C101385 (Begin)  Browse 'Another lineup message'.
      lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
      *C101385 (End) 
      DO CASE
        CASE lnChoice = 3
          RETURN
        CASE lnChoice = 2
          llLineUp =.F.
        OTHERWISE
          SELECT INVHDR
          IF !BOF()
            SKIP-1
          ENDIF  
          EXIT
      ENDCASE
    ENDIF
  ENDSCAN
  IF llLineUp
    LOOP
  ENDIF
  EXIT
ENDDO  
*C101385 (Begin) Add the ENDREPORT function.
DO ENDREPORT

*AHM (Start)
SET DEVICE TO SCREEN
*AHM (End)

*:*************************************************************************
*: PROGRAM   : lfPriHdr
*: DESC.     : PRINT INVOICE Header
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr

*--Initialized the ROW variable in order to move the entire header four lines down.
ROW = 4
lnPageNo = lnPageNo + 1
@ ROW,34 SAY "***INVOICE***"
@ ROW,62 SAY "Date"
@ ROW,71 SAY "Invoice"
ROW = ROW + 1
@ ROW,00 SAY lcDivLName
@ ROW,60 SAY ldInvDate
@ ROW,71 SAY lcInvoice
ROW = ROW + 1

*B803007,1 Suppress the empty lines in Comp. Address(Start)
*@ ROW,00 SAY lcComAdr2
*ROW = ROW + 1
*@ ROW,00 SAY lcComAdr3
*ROW = ROW + 1
*@ ROW,00 SAY lcComAdr4

@ ROW,00 SAY lcComAdr1
ROW = ROW + 1
@ ROW,00 SAY lcComAdr2
ROW = ROW + 1
@ ROW,00 SAY lcComAdr3 + " " + lcComAdr4 + " " + lcComAdr5
*B803007,1 Suppress the empty lines in Comp. Address(End)

ROW = ROW + 1
@ ROW,00 SAY "DUNS# "+lcDuns
ROW = ROW + 2
@ ROW,10 SAY "Bill To :"
@ ROW,45 SAY "Ship To :"
ROW = ROW + 1
@ ROW,10 SAY lcBtName
@ ROW,45 SAY lcStName
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd1
@ ROW,45 SAY lcStAdd1
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd2
@ ROW,45 SAY lcStAdd2
ROW = ROW + 1
@ ROW,10 SAY lcBtAdd3
@ ROW,45 SAY lcStAdd3
ROW = ROW + 2

IF !EMPTY(lcFactor)
  *AHM (Start)
  *ROW = ROW + 1
  *@ ROW , 00 SAY "Remit To :"
  *ROW = ROW + 1
  *@ ROW , 00 SAY "[PAY ONLY IN U.S. FUNDS TO : CONGRESS TALCOTT CORP. P.O. BOX 8500 S 4350,"
  *ROW = ROW + 1
  *@ ROW,  00 SAY "PHILA., PA 19178-4350. THIS AMOUNT AND THE MERCHANDISE COVERED HEREBY IS"
  *ROW = ROW + 1
  *@ ROW , 00 SAY "ASSIGNED TO, OWNED BY, AND IS PAYABLE ONLY TO CONGRESS TALCOTT CORP.,TO WHOM"  
  *ROW = ROW + 1
  *@ ROW , 00 SAY "NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURNS OR ANY CLAIMS FOR SHORTAGE,"   
  *ROW = ROW + 1
  *@ ROW , 00 SAY "NON-DELIVERY OR OTHERWISE WITHIN 10 DAYS AFTER RECEIPT OF GOODS.]"
  *ROW = ROW + 2

  *C101672,1 (Begin) Remark the following lines and print Remit to address properly.
  *ROW = ROW + 1
  *@ ROW , 00 SAY "     This invoice is assigned to , owned by, and payable only to:"
  *ROW = ROW + 1
  *@ ROW,  00 SAY "               The CIT Group/Commercial Services, Inc.           "
  *ROW = ROW + 1
  *@ ROW , 00 SAY "               P.O. Box 1036, Charlotte, NC 28201-1036           "   
  *ROW = ROW + 1
  *@ ROW , 00 SAY "To whom notice must be given of any merchandise returns or claims."
  *ROW = ROW + 1
  *@ ROW , 00 SAY "Payments made to any other party does not constitute valid payment of this invoice."
  *ROW = ROW + 2

  ROW = ROW + 1
  @ ROW , 00 SAY "Remit To :"
  ROW = ROW + 1
  @ ROW , 00 SAY lcNote1
  ROW = ROW + 1           
  @ ROW , 00 SAY lcNote2
  ROW = ROW + 1           
  @ ROW,  00 SAY lcNote3
  ROW = ROW + 1
  @ ROW , 00 SAY lcNote4
  ROW = ROW + 1
  @ ROW , 00 SAY lcNote5
  ROW = ROW + 2
  *C101672,1 (End)  
ENDIF  

@ ROW,00 SAY "Credit Approval #: "+lcApproval
@ ROW,33 SAY "Account : "+lcAccount
@ ROW,51 SAY "P/T# : " +lcPikTkt
@ ROW,70 SAY "Page#: "+ALLTRIM(STR(lnPageNo))
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW,00 SAY "P.O     : " +lcCustPo
@ ROW,30 SAY "Dept.   : " +lcDept
@ ROW,58 SAY "F.O.B   : " + SUBSTR(lcWCity +SPACE(02) +lcWState,1,10)
ROW = ROW + 1
@ ROW,00 SAY "Ship to : " + lcStore
@ ROW,30 SAY "SlsMn   : " + SUBSTR(lcSalesNam ,1,7)
@ ROW,58 SAY "Via     : " + SUBSTR(lcShiVCode ,1,10)
ROW = ROW + 1
@ ROW,00 SAY "Order   : " + lcOrder
@ ROW,58 SAY "Terms   : " + SUBSTR(lcTerVCode ,1,7)
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW,00 SAY "Product             Description                 Tot Qty       Price     Amount  "
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1

*:*************************************************************************
*: PROGRAM   : lfLoadVar (C101209)
*: DESC.     : Load Variables.
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfLoadVar

lcInvoice  = Invoice
lcAccount  = Account
lcStore    = Store
lcRep1     = Rep1
lcSalesNam = IIF(SEEK(lcRep1,"SalesRep"),SalesRep.Name,SPACE(25))
*-C101385  InvDate was replaced in the source 26 program with ShipDate.
ldInvDate  = SHIPDATE
lcOrder    = Order
lcPikTkt   = PikTkt
lcCustPo   = CustPo
lcWareCode = cWareCode
lnAmnt     = ShipAmt
lcFactor   = cFacCode
lnMerch    = ShipAmt
*C101672,1 (Begin) Get the correct freight.
*lnFrei     = Freight
lnFrei      = Freight + INSUR + COD
*C101672,1 (End

lnDisc     = Discount
lnTotChg   = TotalChg
lnShip     = Ship
lcDivision = cDivision
*--- OrderHdr Variables.
=SEEK('O'+lcOrder,'OrdHdr')
lcApproval = OrdHdr.Approval
lcDept     = OrdHdr.Dept
lcNotes1   = OrdHdr.Note1
lcNotes2   = OrdHdr.Note2
*C101385 (Begin) Remark the follwing lines and get them properly in 27.
*lcDivLName = IIF(SEEK('D'+ InvHdr.Division,'Code'),Code.DivLName,SPACE(30))
*lcShiVCode = IIF(SEEK('V'+ InvHdr.ShipVia ,'Code'),Code.cData,SPACE(07))
*lcTerVCode = IIF(SEEK('T'+ InvHdr.Terms   ,'Code'),Code.cData,SPACE(07))
*AHM (Start)
*lcDivLName = gfCodDes(cDivision,PADR('CDIVISION',10))
=gfRltFld(cDivision,@laDisRltFld,'CDIVISION')
*AHM (End)
lcShiVCode = gfCodDes(ShipVia ,PADR('SHIPVIA',10))
lcTerVCode = gfCodDes(cTermCode,PADR('CTERMCODE',10))
*C101385 (End)
lcCusExp   = IIF(EMPTY(lcStore),"M","S") + lcAccount +; 
             IIF(EMPTY(lcStore),"",lcStore)

*AHM Get carton and weight (Start)
lcBIllOfL = IIF(SEEK(lcOrder+lcStore,'pack_hdr'),pack_hdr.Bill_Ladg,"")
lnCart    = InvHdr.Cartons
lnWeig    = InvHdr.Weight
IF llIsCanada
  =SEEK(lcCusExp,'Customer')
  XTAX_RATE = CUSTOMER.nTaxRate
ENDIF
*AHM Get carton and weight (End)

*-- Modify the DUNS# as following :-
*-- A- IF Division = 'L2' OR 'M2' OR 'M3' then print DUNS# as '84-895-9474'
*-- B- IF Division = 'L1' OR 'M1' OR 'M4' then print DUNS# as '62-374-5114'
*-- C- Ohterwise print from the company address
lcDuns  = SPACE(11)
Do Case
  *B803123,1 SHA(Begin)
  *Case INLIST(UPPER(lcDivision), 'L2', 'M2', 'M3')
   Case INLIST(UPPER(lcDivision), 'L2', 'M2', 'M3', 'C2' , 'B2' , 'G2')
    lcDuns = '84-895-9474'

  *Case INLIST(UPPER(lcDivision), 'L1', 'M1', 'M4')  
   Case INLIST(UPPER(lcDivision), 'L1', 'M1', 'M4' , 'C1' ,'B1' , 'G1')  
    lcDuns = '62-374-5114'
  *B803123,1 SHA(End)     
  *AHM (Start)
  CASE INLIST(UPPER(lcDivision),'D1','D2','D3','D4','D5','D6','D7','D8')
    lcDuns ='04-702-2665'
  *AHM (End)
  
  Otherwise
    *C101672,1 (Begin) Get duns with the correct variable name.
    *lcDuns = gfGetMemVar('DUNS')
    lcDuns = gfGetMemVar('XDUNS')
    *C101672,1 (End)
ENDCase
*-- Function to get warehouse city and state
=lfGetWareh()
*-- Ship to Address
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5
=SEEK(lcCusExp,"Customer")
lcBtName = Customer.BtName
=gfGetAdr('Customer','','','',1,'2')
*--Get the Bill To adddess except the country.
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  lcBtAdd&lcCount = lcBtAdd&lcCount + IIF(EMPTY(lcBtAdd&lcCount),'',',')+;
  SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
ENDFOR
*C101672,1 (Begin) Shift the empty address.
  IF EMPTY(lcBtAdd2)
    lcBtAdd2 = lcBtAdd3
    lcBtAdd3 = SPACE(30)
  ENDIF
*C101672,1 (End)
*-- Sold to Address  
*--Get the ship to address with the same no. of chrs that returns
*--in laAddress in Bill to address.
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5
IF SEEK('O'+lcOrder, "OrdHdr") AND OrdHdr.Alt_ShpTo
  lcStName = OrdHdr.StName
  lcStAdd1 = OrdHdr.cAddress1
  lcStAdd2 = OrdHdr.cAddress2
  lcStAdd3 = OrdHdr.cAddress3
ELSE  
  IF SEEK(lcCusExp, "Customer")
    *-- Get the DC address if found.
    lcDist = IIF(!EMPTY(Customer.Dist_Ctr),Customer.Dist_Ctr,' ')
    IF !EMPTY(lcDist)
      =SEEK('S'+lcAccount+lcDist,'Customer')
    ENDIF
    lcStName = IIF(EMPTY(Customer.Dba), Customer.StName, Customer.Dba)
    =gfGetAdr('CUSTOMER','','','',1,'')
    *--Get the Ship To adddess except the country.    
    FOR lnCount = 1 TO ALEN(laAddress,1)
      lcCount = STR(laAddress[lnCount,1],1)
      lcStAdd&lcCount = lcStAdd&lcCount + IIF(EMPTY(lcStAdd&lcCount),'',',')+;
      SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3])
    ENDFOR  
  ENDIF  
ENDIF
*C101672,1 (Begin) Shift the empty address
  IF EMPTY(lcStAdd2)
    lcStAdd2 = lcStAdd3
    lcStAdd3 = SPACE(30)
  ENDIF
*C101672,1 (End)


*:*************************************************************************
*: PROGRAM   : lfClear (C101209)
*: DESC.     : Clear Variable
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfClear

lcInvoice  = SPACE(06)
lcAccount  = SPACE(05)
lcWareCode = SPACE(06)
lcWCity    = SPACE(15)
lcWState   = SPACE(03)
ldInvDate  = {}
*--Initialized the ROW variable in order to move the 
*--entire header four lines down.
ROW        = 4
lnAmnt     = 0
lcDuns     = SPACE(11)
lcSalesNam = SPACE(25) 
lcStore    = SPACE(7)
lcRep1     = SPACE(03)
lcDuns     = SPACE(11)
lcOrder    = SPACE(06)
lnTotChg   = 0
lcDivLName = SPACE(30)
lcNotes1   = SPACE(30)
lcNotes2   = SPACE(30)
lcApproval = SPACE(10)
lcPikTkt   = SPACE(06)
lcCustPo   = SPACE(10)
lcDept     = SPACE(05)
lcShiVCode = SPACE(15)
lcTerVCode = SPACE(15)
lnPageNo   = 0
lnMerch    = 0
lnFrei     = 0
lnDisc     = 0
lnMerch    = 0

*:*************************************************************
*: Name      : lfGetWareh
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: Date      : 12/26/98
*: Purpose   : To get the warehouse information.
*:*************************************************************
*: Example   :  lfGetWareh()
*:*************************************************************
FUNCTION lfGetWareh

*C101672,1 (Begin) Remark the following lines and get the address with the right fields.
*-- To get the warehouse address.
*IF llMultiWH AND SEEK(lcWareCode,'WareHous')
*  lcWCity  = ALLTRIM(WareHous.cCity)
*  lcWState = ALLTRIM(WareHous.cState)
*ELSE
*  GO TOP IN WAREHOUS
*  lcWCity  = ALLTRIM(WareHous.cCity)
*  lcWState = ALLTRIM(WareHous.cState)
*ENDIF
IF llMultiWH AND SEEK(lcWareCode,'WareHous')
  lcWCity  = ALLTRIM(SUBSTR(WareHous.cAddress3,1,15))
  lcWState = ALLTRIM(SUBSTR(WareHous.cAddress4,1,3))
ELSE
  GO TOP IN WAREHOUS
  lcWCity  = ALLTRIM(SUBSTR(WareHous.cAddress3,1,15))
  lcWState = ALLTRIM(SUBSTR(WareHous.cAddress4,1,3))
ENDIF
*C101672,1 (End)

*:*************************************************************************
*: PROGRAM   : lfPriBod (C101209)
*: DESC.     : Print Body
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriBod
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTmpFile)
GOTO TOP
SCAN 
  *-- Say style 
  @ ROW,00 SAY SUBSTR(STYLE,1,lnMajLen)
  *-- Say color
  @ ROW,13 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
  lcStyColD  = IIF(SEEK(SUBSTR(STYLE,1,lnMajLen),'Style'),Style.Desc,SPACE(20)) +;
                  gfCodDes(SUBSTR(STYLE,lnNonMajSt,lnColorLen),'COLOR')
  @ ROW,20 SAY PADR(SUBSTR(lcStyColD,1,28),28)
  @ ROW,50 SAY TotQty       PICTURE '999999'
  @ ROW,57 SAY PRICE        PICTURE '99999999.99'
  @ ROW,68 SAY TotQty*PRICE PICTURE '99999999.99'
  ROW = ROW + 1
  lcScale  = Scale
  lcStyle  = Style  
  lcStyMaj = SUBSTR(STYLE,1,lnMajLen)
  lcColor  = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
  =lfPriScl()
  IF llRpPrtSku
    DO lpPrtSku
  ENDIF
  *C101672,1 (Begin) Corect the condition.
  *IF llRpSkuSize
  IF !llRpSkuSize
  *C101672,1 (End)
    @ ROW,09 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
    lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
    Row = Row + 1
  ELSE
    =lfPrnStrn() 
    lcSkuSize=lcStrToPrn+' '+lcSkuSize
    @ ROW,09 SAY SUBSTR(lcSkuSize,1,86)
    Row = Row + 1  
  ENDIF
  *C101672,1 (Begin) Corect the Row no.
  *IF ROW >=62
  IF ROW >= lnMaxRow
  *C101672,1 (End)
    =lfPriHdr()
  ENDIF
ENDSCAN
Row = Row + 3
IF  SUBSTR(ALLTRIM(lcNotes1),1,1) <>'*';
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes1),1,1)) <>'P';
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes1),2,1)) <>SPACE(01);
    .AND. llRpInvNot
  @ ROW , 00 SAY lcNotes1
  Row = Row + 1
ENDIF
IF  SUBSTR(ALLTRIM(lcNotes2),1,1) <>'*';
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes2),1,1)) <>'P';
    .AND. UPPER(SUBSTR(ALLTRIM(lcNotes2),2,1)) <>SPACE(01);
    .AND. llRpInvNot
  @ ROW , 00 SAY lcNotes2
  Row = Row + 1  
ENDIF
IF  llRpInvNot
 =lfPrnNotPd()
ENDIF
SELECT(lnAlias)

*:*************************************************************************
*: PROGRAM   : lfColctLin (C101209)
*: DESC.     : Colect Lines
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfColctLin

*C101672,1 (Begin) Clear gcDataDir and put the full expression for optimizing.
*SELECT * ;
 FROM (gcDataDir+'InvLine');
 WHERE Invoice = lcInvoice;
       INTO DBF (gcWorkDir+lcTmpFile)
SELECT * ;
 FROM  InvLine;
 WHERE invoice+STR(lineno,6) = lcInvoice;
       INTO DBF (gcWorkDir+lcTmpFile)
*C101672,1 (End)

*:*************************************************************************
*: PROGRAM   : lfPriScl (C101209)
*: DESC.     : Print scales.
*: MODULE    : Aria Apparel Series.
*: DATE      : 12/26/98
*: Developer : Adel Mohammed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriScl
PRIVATE lnAlias

lnAlias = SELECT()
lnIndex = 1
lnPos1  = 0
IF SEEK('S'+lcScale,'Scale')
 FOR lnIndex = 1 TO Scale.CNT
   lnSubStr = ALLTRIM(STR(lnIndex))
   @ ROW,lnPos1   SAY SUBSTR(ALLTRIM(Scale.Sz&lnSubStr),1,3)+"/"
   @ ROW,lnPos1+LEN(SUBSTR(ALLTRIM(Scale.Sz&lnSubStr),1,3)+"/") SAY SUBSTR(ALLTRIM(STR(Qty&lnSubStr)),1,4)
   lnPos1  = lnPos1 + 9
 ENDFOR
 ROW = ROW + 1
ENDIF
SELECT(lnAlias)

*:*************************************************************************
*: PROGRAM     : lpPrtSku.
*: DESC.       : Print the ,main style/color Skus for a specific account.
*: DATE        : 22/04/98
*: Developer   : Adel Mohammed El Gazzar (ADEL)
*: Called from : inv810Z.
*:*************************************************************
PROCEDURE lpPrtSku
PRIVATE lnAlias

lnAlias = SELECT()
*C101385 (Begin) Omit 26 color.
IF !SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
*C101385 (End)
  lcStrToPrn = " "
  RETURN
ELSE
  SELECT Spck_Lin
  IF EMPTY(Sku)
    lnI = 1
    = SEEK('S'+lcScale,'Scale')
    = SEEK('M'+lcAccount,'Customer')
    lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
    IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
      lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
      lnDime2 = SkuTmpl.Len4
    ELSE
      *--Default
      lnDime1 = 8
      lnDime2 = 8
    ENDIF
    *C101385 (Begin) Omit 26 color.
    = SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
    *C101385 (End)
    lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
    lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
    lnLength   = LEN(lcStrToPrn)+2
  ELSE
    @ ROW,00 SAY SUBSTR(Sku,1,8)
  ENDIF
ENDIF
SELECT(lnAlias)

*:***************************************************************************
*: PROGRAM     : lfPrnStrn.
*: DESC.       : Print the main style/color Skus for a specific account.
*: DATE        : 22/04/98
*: Developer   : Adel Mohammed El Gazzar (ADEL)
*: Called from : inv810Z.
*:*************************************************************
FUNCTION lfPrnStrn

lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
*C101385 (Begin) Omit 26 color.
IF SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
  lcKey='S'+lcAccount+lcStyle
  *C101385 (End)
  lnSep=1
  Q=1
  W=STR(Q,1)
  X=1
  Z=STR(X,1)
  SELECT Spck_Lin
  *C101385 (Begin) Omit 26 color.
  *C101672,1 (Begin) Correct the condition.
  *SCAN REST WHILE 'S'+lcAccount+lcStyle = lcKey
  SCAN REST WHILE Type+account+style+pack_id = lcKey
  *C101672,1 (End)
  *C101385 (End)
    IF &lcTmpFile..Qty&Z > 0
        lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
    ENDIF
    lnSep=lnSep+6
    X=X+1
    Z=STR(X,1) 
    Q=Q+1
    W=STR(Q,1)
    IF Z='9'
      EXIT
    ENDIF  
  ENDSCAN
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)
RETURN

*:***************************************************************************
*: PROGRAM     : lfGetLoc..
*: DESC.       : Print the ,main style/color Skus for a specific account.
*: DATE        : 22/04/98
*: Developer   : Adel Mohammed El Gazzar (ADEL)
*: Called from : inv810Z.
*:*************************************************************
FUNCTION lfGetLoc

lcAlias=ALIAS()
lcStr=" "
SELECT WhsLoc
*C101385 (Begin) Omit 26 color.
IF SEEK(lcStyle)
  SCAN REST WHILE Style = lcStyle
*C101385 (End)
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*:***************************************************************************
*: PROGRAM     : lfContuLoc.
*: DESC.       : Print the main style/color Skus for a specific account.
*: DATE        : 22/04/98
*: Developer   : Adel Mohammed El Gazzar (ADEL)
*: Called from : inv810Z.
*:*************************************************************
FUNCTION lfContuLoc

FOR I = 12 TO lnLen
   @ ROW ,00 SAY SUBSTR(lcStr,I,86)
   I=I + 86
   Row = Row + 1
ENDFOR

*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 22/04/98
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
llQuit    = .T.
SET MEMOWIDTH TO 66
SELECT NotePad
IF SEEK ('B' + lcOrder)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    IF ROW >= 60
      lnPageNo = lnPageNo + 1
      =lfPriHdr()
    ENDIF
    IF !EMPTY(MLINE(MNOTES,lnNotLine)) .AND. ;
         (UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) <>'P';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01))
      IF  SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01)
        @ ROW,00 SAY IIF(UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) = 'I' .AND.;
                         UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1)) = SPACE(01) ,;
                         SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,LEN(ALLTRIM(MLINE(MNOTES,lnNotLine)))), ALLTRIM(MLINE(MNOTES,lnNotLine)))
        ROW       = ROW + 1
      ENDIF
    ENDIF   
    lnNotLine = lnNotLine + 1
  ENDDO     
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrnFot
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 22/04/98
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnFot()
*!*************************************************************
FUNCTION lfPrnFot

*AHM Comment this part of code, and rewrite it (Start)

*ROW = 58
*@ ROW,00 SAY REPLICATE('-',78)
*ROW = ROW + 1
*@ ROW ,00 SAY "Total Merchandise : "+STR(lnShip) +"  PCS "+"     $    "+ALLTRIM(STR(lnMerch,9,2))
*ROW = ROW + 1
*@ ROW ,00 SAY "Total Freight     : " +"                     $    "+ALLTRIM(STR(lnFrei,6,2))
*IF lnDisc <> 0
*  ROW = ROW + 1
*  @ ROW ,00 SAY "Total Discount    :" +"                      $    "+ALLTRIM(STR(lnDisc,6,2))
*ENDIF
*ROW = ROW + 1
*@ ROW ,00 SAY "INVOICE AMOUNT    : " +"                     $    "+ALLTRIM(STR(lnTotChg,9,2))
*ROW = ROW + 1  
*@ ROW,00 SAY REPLICATE('-',78)

ROW = 58
@ ROW ,00 SAY "BOL#              :"+lcBIllOfL
ROW = ROW + 1
@ ROW ,00 SAY "NBR Of Cartons    : "+ ALLT(STR(lnCart))
@ ROW ,33 SAY "Weight   : "+ALLT(STR(lnWeig))+ " LBS"
ROW = ROW + 1
@ ROW,00 SAY REPLICATE('-',78)
ROW = ROW + 1
@ ROW ,00 SAY "Total Merchandise : "+ALLTRIM(STR(lnShip)) +" PCS       "
@ ROW ,33 SAY PADL("$"+ALLTRIM(STR(lnMerch,10,2)),10)

IF lnFrei <> 0
  ROW = ROW + 1
  @ ROW ,00 SAY "Total Freight     :"
  @ ROW,33 SAY PADL("$"+ALLTRIM(STR(lnFrei,10,2)),10)
ENDIF
IF lnDisc <> 0
  ROW = ROW + 1
  @ ROW ,00 SAY "Total Discount    :"
  @ ROW,33 SAY PADL("$"+ALLTRIM(STR(lnDisc,10,2)),10)
ENDIF

IF INVHDR.TAX_RATE <> 0
  ROW = ROW + 1
  @ ROW ,00 SAY PADR(ALLT(lcTaxDesc),18) + ": " + ALLTRIM(STR(INVHDR.TAX_RATE,5,2))+'%'
  @ ROW,33 SAY PADL("$"+ALLT(STR(INVHDR->TAX_AMT,10,2)),10)
ENDIF
IF INVHDR.nPSTAmt <> 0
   ROW = ROW + 1
   @ Row,0 SAY 'P S T   T A X' + " : " + ALLT(STR(INVHDR.nPSTRate,5,2))+'%'
   @ ROW,33 SAY PADL("$"+ALLT(STR(INVHDR.nPSTAmt,10,2)),10)
ENDIF
ROW = ROW + 1

@ ROW ,00 SAY "INVOICE AMOUNT    :"
@ ROW,33 SAY PADL("$"+ALLT(STR(lnTotChg,10,2)),10)
ROW = ROW + 1  
@ ROW,00 SAY REPLICATE('-',78)
*AHM Comment this part of code, and rewrite it (End)

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Ahmed Amer (AHM)
*! Date      : 01/25/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : Program code, OG When
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
*B803007,1 Suppress the empty lines in Comp. Address

FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
