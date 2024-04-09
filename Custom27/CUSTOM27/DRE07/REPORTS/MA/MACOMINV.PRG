*:***************************************************************************
*: Program file  : MACOMINV
*: Program desc. : Commercial Invoice Form
*: For Report    : MACOMINV.FRX
*: System        : Aria Advantage Series.
*: Module        : Material (MA)
*: Developer     : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MACOMINV
*:***************************************************************************

DECLARE laVendor[6] , laShipper[6]
laVendor  = ''                   && Array to hold vendor address
laShipper = ''                   && Array to hold the Shipper address
STORE ' ' TO lcShipVia
lcTime     = TIME()              && Variable to hold the Time

SELECT COMINVLN
lcTempFile = gfTempName()
=AFIELDS(laFields)

lnFields = ALEN(laFields,1)+1
DECLARE laFields[lnFields,4]
laFields[lnFields,1] = 'LHASWGT'
laFields[lnFields,2] = 'L'
laFields[lnFields,3] = 1
laFields[lnFields,4] = 0

CREATE CURSOR (lcTempFile) FROM ARRAY laFields
INDEX ON INVOICE+ITEM_TYPE+UOM TAG (lcTempFile) OF (lcTempFile)

SELECT COMINVHD
SCAN FOR &lcRpExp.
  lcUom = SPACE(3)
  llHasWgt = .T.
  IF SEEK(INVOICE,'COMINVLN')
    SELECT COMINVLN
    SCAN REST WHILE invoice+cmattype+pomat+STR(lineno,6,0)+fabric+color=;
                         COMINVHD.INVOICE
      SCATTER MEMVAR
      SELECT (lcTempFile)
      IF SEEK(m.Invoice+m.Item_Type+m.Uom)
        REPLACE NUNITQTY   WITH NUNITQTY + m.nUnitQty,;
                NFABTOTQTY WITH NFABTOTQTY + m.nFabTotQty,;
                NFABWEIGHT WITH NFABWEIGHT + m.nFabWeight,;
                PRICE      WITH PRICE + (m.nFabTotQty * m.Price)
      ELSE
        IF !EMPTY(lcUom) .AND. lcUom # m.Uom
          llHasWgt = .F.
        ENDIF
        lcUom = m.Uom
        APPEND BLANK
        GATHER MEMVAR
        REPLACE CITMDESC WITH gfCodDes(m.Item_Type , 'ITEM_TYPE'),;
                PRICE    WITH (m.nFabTotQty * m.Price)
      ENDIF
    ENDSCAN
  ENDIF
  IF llHasWgt .AND. SEEK(COMINVHD.INVOICE,lcTempFile)
    SELECT (lcTempFile)
    REPLACE REST WHILE INVOICE+ITEM_TYPE+UOM = COMINVHD.INVOICE LHASWGT WITH .T.
  ENDIF
ENDSCAN

SELECT (lcTempFile)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

SET RELATION TO INVOICE INTO COMINVHD
DO gfDispRe WITH EVAL('lcFormName')
USE IN (lcTempFile)
*-- end of Report Code.

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/01/2003
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : Option Grid [Vendor field option]
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend

PRIVATE lcVar, lcObj
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj,.F.,'C')
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*-- end of lfvVend.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/01/2003
*! Purpose   : Generic code to get the old value of the field.
*!*************************************************************
*! Called from : Option Grid (Option that we respect to use its old value)
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvWare
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Valid function of the Warehouse field.
*!*************************************************************
*! Called from : Option Grid [Warehous field option]
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************
FUNCTION lfvWare

PRIVATE lcVar, lcObj
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'WAREHOUS'))
  lcObj = gfBrowWare( .T. )
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*-- end of lfvWare.

*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Valid function of the Container field.
*!*************************************************************
*! Called from : Option Grid [Container field option]
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj, lnAlias
lnAlias = SELECT(0)
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , lcRpCont))
  DECLARE laBrow[1]
  laBrow=' '
  SELECT (lcRpCont)
  lcBrFields = "CCONTAINER :R :H='Container' :30"
  llRetValue = gfBrows('',"CCONTAINER","laBrow",'Containers')
  SELECT (lnAlias)
  IF llRetValue
    &lcVar. = laBrow[1]
  ELSE
    &lcVar. = laOldVal
  ENDIF
ENDIF
*-- end of lfvCont.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : 
*!*************************************************************
*! Called from : ...
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfClearRep

*--Close and remove all tempry files.
IF USED(lcRpCont)
  USE IN ALIAS(lcRpCont)
ENDIF

IF USED(lcRpSeal)
  USE IN ALIAS(lcRpSeal)
ENDIF

IF USED(lcRpBook)
  USE IN ALIAS(lcRpBook)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfvSeal
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Valid function of the Seal field.
*!*************************************************************
*! Called from : Option Grid [Seal field option]
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSeal()
*!*************************************************************
FUNCTION lfvSeal

PRIVATE lcVar, lcObj, lnAlias
lnAlias = SELECT(0)
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , lcRpSeal))
  DECLARE laBrow[1]
  laBrow=' '
  SELECT (lcRpSeal)
  lcBrFields = "CSEAL :R :H='Seal' :30"
  llRetValue = gfBrows('',"CSEAL","laBrow",'Seal')
  SELECT (lnAlias)
  IF llRetValue
    &lcVar. = laBrow[1]
  ELSE
    &lcVar. = laOldVal
  ENDIF
ENDIF
*-- end of lfvSeal.

*!*************************************************************
*! Name      : lfvBook
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Valid function of the Booking field.
*!*************************************************************
*! Called from : Option Grid [Booking field option]
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvBook()
*!*************************************************************
FUNCTION lfvBook

PRIVATE lcVar, lcObj, lnAlias
lnAlias = SELECT(0)
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(lcVar))     && Varible to hold the current field value
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , lcRpBook))
  DECLARE laBrow[1]
  laBrow=' '
  SELECT (lcRpBook)
  lcBrFields = "CBOOKING :R :H='Booking' :30"
  llRetValue = gfBrows('',"CBOOKING","laBrow",'Booking')
  SELECT (lnAlias)
  IF llRetValue
    &lcVar. = laBrow[1]
  ELSE
    &lcVar. = laOldVal
  ENDIF
ENDIF
*-- end of lfvBook.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Optional Grid When Function.
*!*************************************************************
*! Calls     : ...........
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

PRIVATE lnAlias
lnAlias = SELECT(0)

IF !USED(lcRpCont)
  SELECT DISTINCT CCONTAINER;
         FROM COMINVHD;
         ORDER BY CCONTAINER;
         INTO CURSOR (lcRpCont)
  INDEX ON CCONTAINER TAG (lcRpCont) OF (lcRpCont)
ENDIF

IF !USED(lcRpSeal)
  SELECT DISTINCT CSEAL;
         FROM COMINVHD;
         ORDER BY CSEAL;
         INTO CURSOR (lcRpSeal)
  INDEX ON CSEAL TAG (lcRpSeal) OF (lcRpSeal)
ENDIF

IF !USED(lcRpBook)
  SELECT DISTINCT CBOOKING;
         FROM COMINVHD;
         ORDER BY CBOOKING;
         INTO CURSOR (lcRpBook)
  INDEX ON CBOOKING TAG (lcRpBook) OF (lcRpBook)
ENDIF

SELECT WAREHOUS
SET FILTER TO LMATINV
SELECT (lnAlias)
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfHeadVar
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : Page header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfGetAdr,lfAdrShift
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar

lcShipVia = gfCodDes(COMINVHD.ShipVia , 'SHIPVIA')        && Ship Via description.

*-- Fill Vendor array with its data [Begin]
=SEEK(COMINVHD.VENDOR,'APVENDOR')
laVendor[1] = APVENDOR.CVenComp
laVendor[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laVendor[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laVendor[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laVendor[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laVendor[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
= lfAdrShift('laVendor')
*-- Fill Vendor array with its data [End]

*-- Fill Ship to array with its data. [begin]
=SEEK(COMINVHD.CWARECODE,'WAREHOUS')
laShipper[1] = WareHous.cDesc
laShipper[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laShipper[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laShipper[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laShipper[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laShipper[6] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
= lfAdrShift('laShipper')
*-- Fill Ship to array with its data. [End]

RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : AHMED MAHER (AMH)
*! Date      : 01/05/2003
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : MACOMINV.PRG 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
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
*-- end of lfAdrShift.