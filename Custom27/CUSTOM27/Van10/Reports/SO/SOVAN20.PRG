*:************************************************************************
*: Program file  : VAN200.Prg  (C102146,1)
*: Program desc. : Convert Van Mar VAN200 order reports for Target to 2.7
*: Date          : 01/08/2001
*: System        : Aria 27 System 
*: Module        : Sales Order
*: Developer     : Mohamed Shokry (MHM)
*:************************************************************************
*: Calls Procedures : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Modifications      :
*: B605998,1 ALB 09/11/2002 Change total lab in form B
*: B606703,1 ABD 12-10-2002 Fix bug that the report didn't print correct data
*: B606703,1 ABD            in case the user selects report format all .
*: B128514,1 BWA 07/12/2005 Fix bug of "Subscript out of Bounds."
*:****************************************************************************

*-- Section to check if there is account value selected or not.
*-- And the date is not empty.
IF EMPTY(lcRpAcc)
  =gfModalGen('QRM00000B38018','F',' ',' ','Account field is empty')   
  RETURN
ENDIF

IF !EMPTY(lcRpAcc)
  lcRpExp = lcRpExp + " .AND. ACCOUNT = lcRpAcc"
ENDIF  

*--If no recrod return
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
LOCATE FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

OrdHTemp = gfTempName()
OrdLTemp = gfTempName()
TempOrd  = gfTempName()

=gfOpenFile(gcDataDir+'SPCK_LIN',gcDataDir+'SPCKLINS','SH')
=gfOpenFile(gcDataDir+'SKUTMPL',gcDataDir+'SKUTMPL','SH')


lcSkuTmp = Customer.SkuTmpl
lcSkuTmp = IIF(EMPTY(lcSkuTmp),'DEF',lcSkuTmp)
lnSubStr = IIF(SEEK('S'+lcSkuTmp,'SkuTmpl'),SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3+SkuTmpl.Len4,16)


SELECT OrdHdr
COPY ALL TO &gcWorkDir.&OrdHTemp FOR &lcRpExp
=gfOpenFile(gcWorkDir+OrdHTemp,'','EX')
DO CASE 
  CASE lcRpFrmt = 'A'
    DO lpFormA
  CASE lcRpFrmt = 'B'
    DO lpFormB
  CASE lcRpFrmt = 'L'
      DO lpFormA
      ERASE (gcWorkDir+OrdLTemp+'.DBF')
      ERASE (gcWorkDir+OrdLTemp+'.CDX')
      lcRpName  = "SOVAN20B"
      DO lpFormB
ENDCASE


*!*************************************************************
*! Name      : lpFormA
*! Developer : Mohamed Shokry
*! Date      : 01/08/2001
*! Purpose   : Print format A.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : DO lpFormA
*!*************************************************************

PROCEDURE lpFormA

*B606703,1 ABD - Change the form platform in case the user reprint
*B606703,1 ABD - The report for the second time. [Begin]
IF lcRpFrmt = 'L'
  lcRpName  = "SOVAN20A"
  =gfCrtFrm(lcRpName,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpName)
ENDIF
*B606703,1 ABD - [End]

*--Create temp file to add the new record in ordhtemp file if the order have more than 11 stores.
CREATE DBF &gcWorkDir.&TempOrd ; 
              (Order C(6), Account C(5), Flag C(1), Start D ,;
               Complete D, ShipVia C(2), Entered D, CustPo C(15), Labels N(4))

CREATE CURSOR &OrdLTemp ;
              (Order C(6),SKU C(16),STYLE C(19),Qty1 N(5),Qty2 N(5),Qty3 N(5),;
               Qty4 N(5),Qty5 N(5),Qty6 N(5),Qty7 N(5),Qty8 N(5),Qty9 N(5),;
               Qty10 N(5),Qty11 N(5),TotQty N(7),Labels N(4))
INDEX ON Order+ALLTRIM(STR(Labels))+ALLTRIM(SKU)+Style TAG (OrdLTemp)

SELECT &OrdHTemp

*--array to hold the stores per order.
*B128514,1 BWA 07/12/2005 I increase the value dueto the customer has more than 22 DC's.[START]
*DIMENSION laStores[RECCOUNT()*2,11]
DIMENSION laStores[RECCOUNT()*4,11]
*B128514,1 BWA 07/12/2005.[END]

lnPages = 0

SCAN
  *-- Increment the variable that will be used as the first dimension of the array (order number).
  lnPages = lnPages + 1
  REPLACE Labels WITH lnPages

  SELECT OrdLine
  =SEEK('O'+&OrdHTemp..Order)
  WAIT WINDOW ORDER NOWAIT

  *--Initialize a variable to hold no of stores.
  lnStores = 0
  lcStore  = ' '

  SCAN WHILE Order = &OrdHTemp..Order
    lcSku   = ALLTRIM(IIF(SEEK('S'+OrdLine.Account+OrdLine.Style,'SPCK_LIN'),SUBSTR(SPCK_LIN.Pack_ID,1,lnSubStr),SPACE(16)))
    SELECT &OrdLTemp
    *--Collect the stores for each order.
    IF OrdLine.Store <> lcStore
      lcStore  = OrdLine.Store
      lnStores = lnStores + 1
      IF lnStores > 11
        lnStores = 1
        lnPages = lnPages + 1
        SELECT &OrdHTemp
        SCATTER MEMVAR
        SELECT &TempOrd
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Labels WITH lnPages
        SELECT &OrdLTemp
      ENDIF
      laStores[lnPages,lnStores] = OrdLine.Store
    ENDIF
    *-- Add labels to the seek & replace commands.
    IF !SEEK(OrdLine.Order+ALLTRIM(STR(lnPages))+lcSku+OrdLine.Style)
      APPEND BLANK
      REPLACE Order  WITH &OrdHTemp..Order ,;
              Style  WITH OrdLine.Style    ,;
              Sku    WITH lcSku            ,;
              Labels WITH lnPages
    ENDIF
    *-- Get the store no from the variable .
    Z=IIF(lnStores=0,'1',ALLTRIM(STR(lnStores)))
    REPLACE Qty&Z  WITH Qty&Z+OrdLine.TotQty
    REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8+Qty9+Qty10+Qty11
  ENDSCAN

  IF lnStores < 11
    FOR I = lnStores+1 TO 11
      laStores[lnPages,I] = ''
    ENDFOR
  ENDIF
ENDSCAN
*-- Save the new records to the ordhtemp file.
APPEND FROM &gcWorkDir.&TempOrd
SELECT &TempOrd
USE
SELECT &OrdHTemp
INDEX ON ORDER + ALLTRIM(STR(Labels)) TAG &TempOrd
WAIT CLEAR
LOCATE
*-- Add labels to the relation.
SET RELATION TO Order+ALLTRIM(STR(Labels)) INTO &OrdLTemp

SET SKIP TO &OrdLTemp
*B605998,1 ALB totlab in form A [Begin]
IF lcRpFrmt = 'L'
  lcRpName  = "SOVAN20A"
  =gfCrtFrm(lcRpName,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpName)
  SELECT &OrdHTemp
  LOCATE
ENDIF  
*B605998,1 ALB totlab in form A [End]

DO gfDispRe WITH EVAL('lcRpName')
*!*************************************************************
*! Name      : lpFormB
*! Developer : Mohamed Shokry
*! Date      : 01/08/2001
*! Purpose   : Print format B.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : DO lpFormB
*!*************************************************************

PROCEDURE lpFormB
CREATE CURSOR &OrdLTemp ;
              (Order C(6),SKU C(16),Style C(19),TotQty N(7) ,;
               PerBox N(3),Boxes N(6,2),LBS N(6,2),TotLbs N(8,2))
INDEX ON Order+ALLTRIM(SKU)+Style TAG (OrdLTemp)

SELECT &OrdHTemp
*B606703,1 ABD - Dele all the duplicated record from the header file to void 
*B606703,1 ABD - double on the same form. [Begin]
Dele All For EMPTY(cOrdType)
Locate
*B606703,1 ABD - [End]

SCAN 
  SELECT OrdLine
  =SEEK('O'+&OrdHTemp..Order)
  SCAN WHILE ORDER = &OrdHTemp..Order
    lcStyle = Style
    SELECT &OrdLTemp
    lcSku    = ALLTRIM(IIF(SEEK('S'+OrdLine.Account+lcStyle,'SPCK_LIN'),SUBSTR(SPCK_LIN.Pack_ID,1,lnSubStr),SPACE(16)))
    IF !SEEK(&OrdHTemp..Order+lcSku+lcStyle)
      =SEEK(lcStyle,'Style')
      APPEND BLANK
      *B605998,1 ALB Change total lab in form B [Begin]
      *REPLACE Order  WITH &OrdHTemp..Order ,;
              Style  WITH lcStyle          ,;
              SKU    WITH lcSku            ,;
              TotQty WITH OrdLine.TotQty   ,;
              PerBox WITH Style.Qty_Ctn    ,;
              Boxes  WITH IIF(PerBox<>0,TotQty/PerBox,0),;
              LBS    WITH Style.nStyWeight     ,;
              TotLbs WITH Boxes*LBS
      REPLACE Order  WITH &OrdHTemp..Order ,;
              Style  WITH lcStyle          ,;
              SKU    WITH lcSku            ,;
              TotQty WITH OrdLine.TotQty   ,;
              PerBox WITH Style.Qty_Ctn    ,;
              Boxes  WITH IIF(PerBox<>0,TotQty/PerBox,0),;
              LBS    WITH Style.nStyWeight     ,;
              TotLbs WITH TotQty*LBS
      *B605998,1 ALB Change total lab in form B [End]
    ELSE
      *B605998,1 ALB Change total lab in form B [Begin]
*      REPLACE TotQty WITH TotQty + OrdLine.TotQty ,;
              Boxes  WITH IIF(PerBox<>0,TotQty/PerBox,0),;
              TotLbs WITH Boxes*LBS              

      REPLACE TotQty WITH TotQty + OrdLine.TotQty ,;
              Boxes  WITH IIF(PerBox<>0,TotQty/PerBox,0),;
              TotLbs WITH TotQty*LBS              
      *B605998,1 ALB Change total lab in form B [End]
    ENDIF
  ENDSCAN
ENDSCAN
SELECT &OrdHTemp
LOCATE
SET RELATION TO Order INTO &OrdLTemp
SET SKIP TO &OrdLTemp

IF lcRpFrmt = 'L'
  lcRpName  = "SOVAN20B"
  =gfCrtFrm(lcRpName,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpName)
  SELECT &OrdHTemp
  LOCATE
ENDIF  
DO gfDispRe WITH EVAL('lcRpName')

*!*************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/08/2001
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    SET ORDER TO Customer IN Customer
    SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store);
           INTO CUSTOMER && To customer file.
    LOCATE       
  
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE cOrdType = "O" 
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "O" 
      CASE cOrdType = "C"
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "C" 
      CASE cOrdType  = "T" 
            SET FILTER TO ACCOUNT= lcRpAcc .AND. STATUS $'OHB' 
            LOCATE FOR CORDTYPE+ORDER = "T" 
    ENDCASE
    
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    SET FILTER TO 
ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/08/2001
*! Purpose   : Validate account in CUSTOMER file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAcc()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field


*-- IF The user want to Browse or if the Account he entered is not in the file
IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.                  && Browse for all cases not click the botton
  lcRpAcc = lcObjVal
  DO CUSBROWM WITH lcRpAcc
  lcObjVal = lcRpAcc
  llBrowse = .F.
ENDIF
&lcObjNam = lcObjVal

IF lcRpAcc # laOldVal
  llClearOrd = .T.
ELSE
  llClearOrd = .F.
ENDIF

*--End of lfvAcct.
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/08/2001
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfVForm
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/08/2001
*! Purpose   : validate function to get Form name
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfVForm
DO CASE 
  CASE lcRpFrmt $ 'AL'
    lcRpName  = "SOVAN20A"
  CASE lcRpFrmt = 'B'
    lcRpName  = "SOVAN20B"
ENDCASE
*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/08/2001
*! Purpose   : Function to Get the Description of the Ship Via
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfCodDes
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr
*B605998,1 ALB Get Ship Via Description [Begin]
*lcShipVia = gfCodDes(ordhdr.ShipVia , 'SHIPVIA'  )
lcShipVia = gfCodDes(ShipVia , 'SHIPVIA'  )
*B605998,1 ALB Get Ship Via Description [end]
*--End of lfSolSpAdr
*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))


IF !EMPTY(lcObj) AND ('?' $ lcObj OR SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') OR SEEK('T'+lcObj , 'ORDHDR') )

  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = "Orders/Contracts"

  lcBrowCond = [FOR ACCOUNT= lcRpAcc .AND. STATUS $'OHB' ]
  
  = gfBrows(lcBrowCond,'ORDER','laTemp')  
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvOrder.
****************************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 01/11/2001
*! Purpose   : When function to get the Old value
****************************************************************************
*! Called from : Some of the Option Grid fields
****************************************************************************
*! Calls       : None
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : None
****************************************************************************
*! Example     : = lfwOldVal()
****************************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- end of lfwOldVal.
