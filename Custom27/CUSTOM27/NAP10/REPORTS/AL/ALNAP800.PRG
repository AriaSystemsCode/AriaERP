*:****************************************************************************************
*: Program file  : ALNAP800.PRG (REFERE TO C102338)
*: Program desc. : Manifest Report For Nap.
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation (AL).
*: Developer     : Ahmed Abdel Naby - (AAN)
*: Date          : 05/28/2001
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:

*-- Check if the system is using multi warehouse or single warehouse.
PRIVATE lcAccName,lcPickDesc

DECLARE laSoldTo[5,1] , laShipTo[5,1]
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

STORE '' TO lcAccName , lcPickDesc
IF 'BOL_HDR.ACCOUNT' $ lcRpExp
  SELECT Customer
  =SEEK('M'+laOGFxFlt[1,6])
  lcAccount = Customer.Account
  lcAccName = Customer.StName
  SELECT BOL_HDR
  *-- Initializing the necessary variables
  Row      = 0
  lcBrTit  = 'BILL OF LADING'
  *-- Checking if the system is using Edi module.
  llEdiSys = (OCCURS('EB',gcCmpModules)<>0)
  *-- Opening the EdiAcc file if the Edi Module exists
  IF llEdiSys
    =gfOpenFile(gcDataDir+'EdiAcPrt',gcDataDir+'Partner','SH')
  ENDIF
  *-- To print the repot
  =lfvCollect()
  SELECT(lcTmpBlHd)
  GO TOP
  IF EOF()
    =gfModalGen('TRM00052B00000','DIALOG' )
  ELSE
    IF llRpPickUp
      =lfvPickUp()
    ENDIF
    =lfPrntRep()
  ENDIF
ELSE
  =gfModalGen('TRM44107B00000','DIALOG' )
ENDIF
SET DEVICE TO SCREEN
RETURN

*:*************************************************************
*: Name      : lfPrntRep
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*: Purpose   : To Print the report.
*:*************************************************************
FUNCTION lfPrntRep

*-- Selecting the Sorting way
DO CASE
  CASE lcRpSortBy = "S"
    lcOrder = "&lcTmpBlHd..Bol_No,Pack_Hdr.Store,Pack_Hdr.Order"
  CASE lcRpSortBy = "O"
    lcOrder = "&lcTmpBlHd..Bol_No,Pack_Hdr.Order,Pack_Hdr.Store"
  CASE lcRpSortBy = "P"
    lcOrder = "&lcTmpBlHd..Bol_No,Bol_Lin.Pack_No,Pack_Hdr.Store"
ENDCASE    

*-- To select all the Bol lines 
SELECT &lcTmpBlHd..Bol_No,&lcTmpBlHd..cPickDesc,Bol_Lin.Pack_No,;
       Pack_Hdr.Order,Pack_Hdr.Store;
  FROM (lcTmpBlHd),BOL_Lin,Pack_hdr;
  WHERE &lcTmpBlHd..Bol_No = Bol_Lin.Bol_No;
  AND Bol_lin.Pack_No = Pack_Hdr.Pack_No;
  INTO CURSOR (lcTmpBlLn);
  ORDER BY &lcOrder
  
SELECT(lcTmpBlLn)
INDEX ON Bol_No TAG (lcTmpBlLn)

SELECT (lcTmpBlLn)
GO TOP
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
ELSE
  *-- initializing the necessary variables
  R_WIDTH   = 'W'
  PAGENO    = 0
  lcBolNo   = SPACE(6)
  *-- To check if the Edi module exists and the account is an Edi one.
  llPrint = llEdiSys .AND. SEEK(lcAccount,'EdiAcPrt')
  SET DEVICE TO SCREEN
  WAIT WINDOW "Report printing - <SPACE BAR> to abort" NOWAIT
  SET DEVICE TO PRINT
  SELECT (lcTmpBlLn)
  DO WHILE !EOF() AND INKEY() <> 32  
    =SEEK(Bol_No,lcTmpBlHd)
    *-- To print the header for each BOL
    =lfPrntHdr()
    *-- Variables to hold the totals of a BOL
    STORE 0 TO lnTotQty,lnTotCart,lnTotWght
    *-- To print all the Packing # for the selected BOL
    SELECT (lcTmpBlLn)
    lcBolNo = Bol_No

    =SEEK(lcBolNo)
    SCAN REST WHILE Bol_No = lcBolNo

      lcDummy = IIF(Row >= 55,lfPrntHdr(),.F.)
      lcDummy = SEEK(Pack_No,'Pack_Hdr')
      *=SEEK('S'+lcAccount+Pack_Hdr.Store,'Customer')
      =SEEK('O'+Order,'OrdHdr')
      IF OrdHdr.MultiPo
        =SEEK('O'+Order+Pack_Hdr.Store,'OrdLine')
      ENDIF
      @ Row,00  SAY Pack_Hdr.Store
      =lfGetAddr()
      @ Row,10  SAY LEFT(laShipTo[1],30)
      @ Row,41  SAY LEFT(laShipTo[3],16)
      @ Row,58  SAY LEFT(laShipTo[4],3)
      @ Row,62  SAY LEFT(laShipTo[5],10)
      @ Row,73  SAY OrdHdr.Dept
      @ Row,79  SAY Pack_Hdr.Tot_Pcs PICTURE "99999999"
      lnTotQty = lnTotQty + Pack_Hdr.Tot_Pcs
      @ Row,88  SAY Pack_Hdr.Tot_Cart PICTURE "99999999"
      lnTotCart = lnTotCart + Pack_Hdr.Tot_Cart
      @ Row,98 SAY Pack_Hdr.Tot_Wght PICTURE "99999999"
      lnTotWght = lnTotWght + Pack_Hdr.Tot_Wght
      @ Row,107 SAY IIF(OrdHdr.MultiPo,LEFT(OrdLine.CustPo,10),LEFT(OrdHdr.CustPo,10))
      @ Row,118 SAY Order
      @ Row,125 SAY Pack_Hdr.Pack_no
      Row = Row + 1
    ENDSCAN
    Row = Row + 1
    @ Row,00 SAY REPLICATE("-",132)
    Row = Row + 1
    @ Row,60 SAY "TOTAL :"
    @ Row,78 SAY lnTotQty         PICTURE "999999999"  
    @ Row,87 SAY lnTotCart        PICTURE "999999999"
    @ Row,97 SAY lnTotWght PICTURE "999999999"
    Row = Row + 1
    @ Row,00 SAY REPLICATE("-",132)
  ENDDO
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF


*:*************************************************************
*: Name      : lfPrntHdr
*: Developer : Ahmed Abdel Naby - (AAN)
*: Date      : 05/28/2001
*: Purpose   : To print the report header
*:*************************************************************
FUNCTION lfPrntHdr

DECLARE laCmAdr[5,1]
laCmAdr = ''           && Array to hold the warehous address.

*-- Get the WareHouse addresses
laCmAdr[1] = gfGetAdr('WareHous' , '' , '' , '' , 1)
laCmAdr[2] = gfGetAdr('WareHous' , '' , '' , '' , 2)
laCmAdr[3] = gfGetAdr('WareHous' , '' , '' , '' , 3)
laCmAdr[4] = gfGetAdr('WareHous' , '' , '' , '' , 4)
laCmAdr[5] = gfGetAdr('WareHous' , '' , '' , '' , 5)
lcWHAdd1 = ALLTRIM(laCmAdr[1])
lcWHAdd2 = ALLTRIM(laCmAdr[2])
lcWHAdd3 = TRIM(laCmAdr[3]) + ' ' + laCmAdr[4] + ' '+ laCmAdr[5]
IF EMPTY(lcWHAdd2)
  lcWHAdd2 = lcWHAdd3
  lcWHAdd3 = ''
ENDIF      

PAGENO   = PAGENO + 1
lnLineX1  = ((130 - (LEN(TRIM(QCOMPANY))))/2)
lnLineX2 = ((130 - (LEN(lcWHAdd1)))/2)
lnLineX3 = ((130 - (LEN(lcWHAdd2)))/2)
lnLineX4 = ((130 - (LEN(lcWHAdd3)))/2)
lnLineX5 = ((130 - (LEN('Manifest for '+ALLTRIM(lcAccName))))/2)
lnLineX6 = ((130 - (LEN('Supplier# '+IIF(llPrint,ALLTRIM(EdiAcc.Vend),'')+;
                      IIF(llRpPickUp,'   Pickup# '+;
                      +ALLTRIM(&lcTmpBlHd..cPickDesc),''))))/2)

@ 01,lnLineX1 SAY gcCom_Name
@ 02,lnLineX2 SAY lcWHAdd1
@ 03,lnLineX3 SAY lcWHAdd2
IF !EMPTY(lcWhAdd3)
  @ 04,lnLineX4 SAY lcWHAdd3
ENDIF
@ IIF(!EMPTY(lcWhAdd3),5,4),00       SAY "NAP800"
@ IIF(!EMPTY(lcWhAdd3),5,4),lnLineX5 SAY 'Manifest for '+ALLTRIM(lcAccName)
@ IIF(!EMPTY(lcWhAdd3),5,4),120      SAY gdSysDate
@ IIF(!EMPTY(lcWhAdd3),5,4),31       SAY '~'
@ IIF(!EMPTY(lcWhAdd3),6,5),00       SAY TIME()
@ IIF(!EMPTY(lcWhAdd3),6,5),lnLineX6 SAY IIF(llPrint,'Supplier# '+ALLTRIM(EdiAcc.Vend),'')+;
                                         +IIF(llRpPickUp,'   Pickup# '+;
                                         ALLTRIM(&lcTmpBlHd..cPickDesc),'')

@ IIF(!EMPTY(lcWhAdd3),6,5),120 SAY 'PAGE#'
@ IIF(!EMPTY(lcWhAdd3),6,5),126 SAY STR(PAGENO,4)
@ IIF(!EMPTY(lcWhAdd3),7,6),00 SAY REPLICATE('*',132)

Row = IIF(!EMPTY(lcWhAdd3),8,7) 
@ Row,00 SAY 'Store#    Address                        City             ST  Zip        Dept.      Qty    Crtn.      Wgh. PO#        SO#    PT#'
Row = Row + 1
@ ROW,00 SAY REPLICATE('-',132)
Row = Row + 1

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : Validation function for the Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

PRIVATE lcObjName , lcObjVal , llObjRet , lcAlias , lcOrdr

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , '')
  lcAlias = SELECT(0)
  SELECT BOL_HDR
  lcOrdr = SET('ORDER')
  SET ORDER TO TAG BOLACC
  GO TOP
  IF SEEK(lcObjVal)
    &lcObjName = lcObjVal
  ELSE
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'There are no BOLs for '+ALLTRIM(lcObjVal)+'. Cannot proceed.')  
  lcObjVal = ''
  &lcObjName = lcObjVal
  ENDIF
  SET ORDER TO &lcOrdr
  SELECT (lcAlias)
ENDIF

*!**************************************************************************
*! Name      : lfsAccount
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsAccount()
*!**************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!**************************************************************************
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF    
*-- End of lfsAccount.

*!***************************************************************************
*! Name      : lfSRBillNo
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 19/06/2001
*! Purpose   : control browse BOL_No for Bill of Lading file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRBillNo()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRBillNo
PARAMETERS lcParm

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT BOL_Hdr

DO CASE
  CASE lcParm = 'S'  && Set code
    GO TOP
  CASE lcParm = 'R'  && Reset code
    llClearBno = .F.
    SELECT (lnAlias)
ENDCASE
*-- End of lfSRBillNo.
*!*************************************************************
*! Name      : lfvCollect
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To collect the data
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvCollect

SELECT *, SPACE(15) AS cPickDesc, SPACE(1) AS FLAG;
  FROM Bol_Hdr ;
  WHERE &lcRpExp ;
  INTO DBF (gcWorkDir + lcTmpBlHd + '.DBF');
  ORDER BY Bol_No
SELECT(lcTmpBlHd)
INDEX ON Bol_No TAG (lcTmpBlHd)
*-- End of lfvCollect.

*!*************************************************************
*! Name      : lfvPickUp
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To validate the pickup option.
*!*************************************************************
FUNCTION lfvPickUp
*-- If the user want to assign PickUp# to the BOL
IF llRpPickUp
  GOTO TOP IN (lcTmpBlHd)
  lcPickDesc = &lcTmpBlHd..cPickDesc
  DO (gcRepHome+"NAP800_1.SPX")
ENDIF  

*!*************************************************************
*! Name      : lfvClose
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To validate the Close push button
*!*************************************************************
*! Example            :  lfvClose()
*!*************************************************************
FUNCTION lfvClose

lcPickDesc = SPACE(15)
=gfClearKey()
CLEAR READ

*!*************************************************************
*! Name      : lfBrows
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To brows the selected range of BOL#.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows

PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpBlHd)
GOTO TOP
DO CASE
  CASE _DOS .OR. _UNIX
    BROWSE FIELDS ;
       Bol_No    :H = 'BOL#' :6:R,;
       cPickDesc :H = 'PickUp #':29:R;
       WINDOW    NAP800A  ;
       IN WINDOW NAP800_1 ;
       WHEN lfwBrow()    ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       COLOR SCHEME 10   ;
       TITLE lcBrTit
  CASE _WINDOWS .OR. _MAC
    BROWSE FIELDS ;
       Bol_No :H = 'BOL#'  :R,;
       cPickDesc :H = 'PickUp #':30:R;
       WINDOW    NAP800A  ;
       IN WINDOW NAP800_1;
       WHEN lfwBrow()    ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       TITLE lcBrTit
ENDCASE
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To be proceesed when moving into the browse
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

lcPickDesc    = &lcTmpBlHd..cPickDesc
SHOW GET lcPickDesc
=lfRefresh('NAP800B') 

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 


*!*************************************************************
*! Name      : lfvDeact
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : The READ Deactivate function of screen NAP800
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfvDeact

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'NAP800B', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'NAP800B', 'lcPickDesc'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'NAP800B', 'PbClose'
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvGetPick
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 07/19/2001
*! Purpose   : To validate pickup number.
*!*************************************************************
*! Example            :  lfvGetPick()
*!*************************************************************
FUNCTION lfvGetPick

REPLACE cPickDesc WITH lcPickDesc
SHOW WINDOW (lcBrTit) REFRESH SAME


*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 08/06/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
FUNCTION lfGetAddr
PARAMETER llDummy

lcCurrAlis = ALIAS()
laSoldTo = ''
laShipTo = ''


IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  
ELSE    && Else
  =SEEK('S'+lcAccount+Pack_Hdr.Store,'Customer')
  
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + lcAccount + lcDistCntr
  ELSE
    =SEEK('S' + lcAccount + Pack_Hdr.Store))
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + Customer.Phone1

ENDIF    && End of IF

SELECT (lcCurrAlis)

RETURN ''
*-- End of lfGetAddr
