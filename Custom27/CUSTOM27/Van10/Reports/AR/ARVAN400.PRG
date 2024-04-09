*:***************************************************************************
*: Program file  : ARVAN400.PRG
*: Program desc. : CUSTOMIZED ORDER REPORT FOR VAN MAR.(REFERE TO C#101272)
*: Date          : 01/21/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C102148
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARVAN400
*:***************************************************************************

*-- Section to check if there is account value selected or not.
*-- And the date is not empty.
IF EMPTY(lcAccount)
  WAIT WINDOW "Account Field Is Empty  "
  SET DEVICE TO SCREEN
  RETURN
ENDIF

STORE .F. TO llFlgBrwVn , llReturn
IF !SEEK('M'+lcAccount , 'CUSTOMER')
  llFlgBrwVn = .T.
  =lfvAcct()
  RETURN
ENDIF

IF ldRpTDate = {}
  WAIT WINDOW "You must enter the date range..!! Please Retry" 
  RETURN
ENDIF

*--- Section for the global addresses and name of the company.
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompFax  = cCom_Fax              && Variable to hold the Company Fax
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone

DECLARE laCompAdd[6,1],laAccDc[11]
laCompAdd = ''          && Array to hold the Company address
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)

*--- Variable declaration
lcOrderVm = ""             &&Variable hold the return value of the user criteria for the orders.
lcSKU     = SPACE(08)      &&Variable hold the Sku Value.
lcSiz     = ''
lnTotShip = 0
lcStyle   = SPACE(10)      &&Variable hold the Style Value.

*--- Define array to hold 11 Dc's
DIMENSION laAccDc[11]
laAccDc = SPACE(01)

*---START MAIN PROGRAM
SELECT ORDHDR
IF lcRpSlBy = 'O'
  WAIT WINDOW 'Collecting Orders Records...' NOWAIT
  =lfVOrder()
  IF llReturn
    IF EMPTY(lcOrderVm)
      SELECT Account,ORDER,FLAG;
        FROM ORDHDR INTO TABLE (gcWorkDir+lcOrdhTmp);
        WHERE cordtype = 'O'      ;
        .AND. Account = lcAccount ;
        .AND. Status <> 'X'       ;
        .AND. Open <> 0           ;
        ORDER BY ORDER
    ELSE
      SELECT Account,ORDER,FLAG;
        FROM ORDHDR INTO TABLE (gcWorkDir+lcOrdhTmp);
        WHERE cordtype = 'O'      ;
        .AND. IIF("BETWEEN(" $ lcOrderVm , &lcOrderVm , ORDER $ lcOrderVm );
        .AND. Account = lcAccount ;
        ORDER BY ORDER
    ENDIF
  ENDIF
ELSE   && case the user select sort by (Date)
  llReturn = .T.
  SELECT * FROM ORDHDR INTO TABLE (gcWorkDir+lcOrdhTmp);
    WHERE ORDHDR.OPEN > 0      ;
    .AND. ORDHDR.STATUS <> 'X' ;
    .AND. IIF(ldRpFDate ={} , ENTERED <= ldRpTDate , BETWEEN(ENTERED,ldRpFDate,ldRpTDate));
    .AND. IIF(EMPTY(lcRPSason)  , SEASON = "" , SEASON = lcRPSason );
    .AND. IIF(EMPTY(lcRpDivson) , CDIVISION = "" , CDIVISION = lcRpDivson );
    .AND. Account = lcAccount ;
    ORDER BY ORDER

ENDIF

IF llReturn
  SELECT (lcOrdhTmp)
  IF _TALLY = 0
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
ENDIF

*--- Function to collect DC's of the selected account
IF llReturn
  = lfSelectDc()
ENDIF

*--- Function to collect and prepare data for printing
IF llReturn
  IF !lfColOrdL() .OR. !lfColData()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF

  *--- Start printing report
  SELECT (lcTmpToPr)
  SET RELATION TO "O" + ORDER INTO ORDHDR
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF

IF USED('SYCINT')
  SELECT SYCINT
  USE
ENDIF
WAIT CLEAR
SET DEVICE TO SCREEN
                      *-- END MAIN PROGRAM --*
*!*************************************************************
*! Name      : lfColOrdL
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : FUNCTION TO COLLECT ORDER LINES + STYLE/COLOR GROUP.
*!*************************************************************
*! Called from : Program
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColOrdL()
*!*************************************************************
FUNCTION lfColOrdL

PRIVATE lnAlias
lnAlias = SELECT(0)

CREATE DBF (gcWorkDir+lcColOrdL) ;
                        (STYLE   C(19)  , ORDER  C(06) , OTOTQTY N(8,0) ,;
                         SGROUP C(06) , DC C(08))

INDEX ON ORDER + DC + STYLE TAG (lcColOrdL)
SELECT (lcOrdhTmp)

SCAN
  lcOrder = ORDER
  SELECT ORDLINE
  = SEEK('O'+lcOrder)
  SCAN REST WHILE cordtype+ ORDER + STR(LINENO,6) = 'O' + lcOrder
    = SEEK(Style ,'STYLE')
    SELECT(lcColOrdL)
    APPEND BLANK
    REPLACE STYLE   WITH ORDLINE.Style   ;
            ORDER   WITH ORDLINE.ORDER   ;
            OTOTQTY WITH ORDLINE.TOTQTY  ;
            SGROUP  WITH STYLE.cstygroup ;
            DC      WITH ORDLINE.STORE
  ENDSCAN
ENDSCAN
GOTO TOP IN (lcColOrdL)
SELECT (lnAlias)
RETURN(!EOF(lcColOrdL))

*-- End of lfColOrdL.
*!*************************************************************
*! Name      : lfGetSku
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : FUNCTION TO GET SKU.
*!*************************************************************
*! Called from : Program
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetSku()
*!*************************************************************
FUNCTION lfGetSku
PRIVATE lcAlias

lcAlias = SELECT(0)
IF !SEEK('S'+lcAccount+lcStyle,'Spck_Lin')
  lcSku = SPACE(08)
  RETURN
ENDIF

= SEEK('M'+lcAccount,'Customer')
lcSkuTmpl = IIF(EMPTY(Customer.SkuTmpl),'DEF',Customer.SkuTmpl)
IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = MAX(SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3,8)
ELSE
  lnDime1 = 8  &&Default
ENDIF
lcSku = SUBSTR(Spck_Lin.Pack_Id,1,lnDime1)
SELECT(lcAlias)

*--End of lfGetSku.
*!*************************************************************
*! Name      : lfGetScDec
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : FUNCTION TO GET STYLE SIZES.
*!*************************************************************
*! Called from : Program
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetScDec()
*!*************************************************************
FUNCTION lfGetScDec
PRIVATE lcAlias

lcAlias = SELECT(0)
= SEEK(lcStyle ,'STYLE') AND SEEK('S'+ STYLE.SCALE,'SCALE')
lcSiz = ALLTRIM(SCALE.CSCL_DESC)
SELECT(lcAlias)

*--End of lfGetScDec.
*!*************************************************************
*! Name      : lfSelectDc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : FUNCTION TO GET ACCOUNT DC's.
*!*************************************************************
*! Called from : Program
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSelectDc()
*!*************************************************************
FUNCTION lfSelectDc

SELECT DIST STORE FROM CUSTOMER WHERE TYPE+Account+STORE = 'S'+lcAccount;
 AND !EMPTY(STORE) ORDER BY STORE INTO ARRAY laAccDc

*---TO REDIMENTION THE ARRAY ACCOURDING TO NO OF DC's IF
*---LESS THAN 11 REDIM. AS 11 AND FILL REMAINDER AS SPACE
*---ELSE REDIM. AS 11 TO TAKE ONLY FIRST 11 DC's
lnLen = ALEN(laAccDc)
IF ALEN(laAccDc) < 11
  DECLARE laAccDc[11]
  FOR lnIndex = lnLen + 1 TO 11
    laAccDc[lnIndex] = SPACE(01)
  ENDFOR
ELSE
  DECLARE laAccDc[11]
ENDIF

*--End of lfSelectDc.
*!*************************************************************
*! Name      : lfColData
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : FUNCTION TO PREPARE DATA FOR PRINTING.
*!*************************************************************
*! Called from : Program
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColData()
*!*************************************************************
FUNCTION lfColData
PRIVATE lnAlias,lnQty

lnAlias = SELECT(0)
lnQty   = 0

CREATE DBF (gcWorkDir+lcTmpToPr);
                       (SGROUP C(06) , ORDER C(06)  , SSKU  C(08) ,;
                        STYLE  C(19) , SSIZE C(10) ,;
                        D1 N(8,0) , D2   N(8,0) , D3  N(8,0) ,D4 N(8,0),;
                        D5 N(8,0) , D6   N(8,0) , D7  N(8,0) ,D8 N(8,0),;
                        D9 N(8,0) , D10  N(8,0) , D11 N(8,0))

INDEX ON SGROUP+ORDER+SSKU+STYLE TAG (lcTmpToPr)
SELECT (lcColOrdL)
GOTO TOP

DO WHILE !EOF()
  lcOrder = Order
  lcStyle = Style
  lcDc    = Dc
  lcGroup = SGroup

  SUM REST OTOTQTY WHILE ORDER   + DC   + STYLE   =;
                         lcOrder + lcDc + lcStyle TO lnQty
  lcDcNo    = ALLTRIM(STR(ASCAN(laAccDc,lcDc)))
  IF lcDcNo = '0'
    LOOP
  ENDIF

  SELECT (lcTmpToPr)
  *---Function to get SKU#
  = lfGetSku()

  *---Function to get SCALE DESC.
  = lfGetScDec()

  IF !SEEK(lcGroup + lcOrder  + lcSku + lcStyle)
    APPEND BLANK
    REPLACE ORDER  WITH lcOrder;
            STYLE  WITH lcStyle;
            SGroup WITH lcGroup;
            SSIZE  WITH lcSiz  ;
            SSKU   WITH lcSKU
  ENDIF
  REPLACE D&lcDcNo WITH D&lcDcNo + lnQty
  SELECT (lcColOrdL)
ENDDO
GOTO TOP IN (lcTmpToPr)
SELECT(lnAlias)
RETURN (!EOF(lcTmpToPr))

*--End of lfColData.
*!*************************************************************
*! Name      : lfvAcct
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : Validate the acount from the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvAcct()
*!*************************************************************
FUNCTION lfvAcct
PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user want to Browse or if the Account he entered is not in the file
IF llFlgBrwVn
  lcObjVal = lcAccount
ENDIF

IF !EMPTY(lcObjVal) .AND. !SEEK('M'+lcObjVal , 'CUSTOMER')
  llBrowse = .T.                  && Browse for all cases not click the botton
  lcAccount = lcObjVal
  DO CUSBROWM WITH lcAccount
  lcObjVal = lcAccount
  llBrowse = .F.
ENDIF
&lcObjNam = lcObjVal

IF llFlgBrwVn
  llFlgBrwVn = .F.
  _CUROBJ = OBJNUM(lcAccount)
ENDIF

*--End of lfvAcct.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls     : DateRng.Spx
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return    : None
*!*************************************************************
*! Example   : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldRpFDate
ldTo   = ldRpTDate

lcTitle = 'Entered Date'
DO DateRng.Spx

ldRpFDate = ldFrom
ldRpTDate = ldTo

*-- End of lfvDateRng.
*!*************************************************************
*! Name      : lfvPbOk
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldRpFDate = ldFrom
  ldRpTDate = ldTo
  CLEAR READ
ENDIF

*--End of lfvPbOk.
*!*************************************************************
*! Name      : lfClrRead
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : To refresh the option grid.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClrRead()
*!*************************************************************
FUNCTION lfClrRead

CLEAR READ

*--End of lfClrRead.
*!*************************************************************
*! Name      : lfVOrder
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/17/2000
*! Purpose   : Brow the order inrange to get the user select orders.
*!*************************************************************
*! Called from : ARVAN400.PRG
*!*************************************************************
*! Calls       : gfrange,gfGenFlt
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfVOrder()
*!*************************************************************
FUNCTION lfVOrder

PRIVATE lcAlias
lcAlias = ALIAS()
STORE SPACE(0) TO lcExpsty

lcDevice = SET('DEVICE')
SET DEVICE TO SCREEN

CREATE CURSOR (lcTempOrdr) (Order C(6))
ZAP
INDEX ON Order TAG (lcTempOrdr) OF (gcWorkDir+lcTempOrdr+'.CDX')

lcBrowFlds = [Order     :H = 'Order'       :7   ,]                    + ;
             [Season    :H = 'Season'      :7   ,]                    + ;
             [cDivision :H = 'Division'    :9   ,]                    + ;
             [Status    :H = 'Status'      :6   ,]                    + ;
             [Account   :H = 'Account'     :8   ,]                    + ;
             [Store     :H = IIF(MULTI='Y', '*MULTI*', STORE)  :8  ,] + ;
             [CUSTOMER.STNAME :H = 'Name'  :25  ,]                    + ;
             [Start     :H = 'Start'       :10  ,]                    + ;
             [COMPLETE  :H = 'Complete'    :10  ,]                    + ;
             [OPENAMT   :H = 'Open'        :12  ,]                    + ;
             [APPRAMT   :H = 'Aprve'       :12  ,]                    + ;
             [Bulk      :H = IIF(Bulk = "Y","Yes","No") :4  ]


SELECT ORDHDR
lcExpsty = 'FOR ' + " Status <> 'X' .AND. Open <> 0 .AND. Account = lcAccount "
llReturn = gfrange(lcBrowFlds,lcTempOrdr,"ORDER",(lcExpsty),"","","@! XXXXXX")

*-- Get the original setting of the device.
SET DEVICE TO &lcDevice

DIMENSION laStylFle[1,7]
laStylFle[1,1]= "ORDHDR.ORDER"
laStylFle[1,2]= "F"
laStylFle[1,3]= "C"
laStylFle[1,4]= .T.
laStylFle[1,5]= "In List             "
laStylFle[1,6]= lcTempOrdr
laStylFle[1,7]= "R"

DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]
=ACOPY(laBrFldFlt,laBrTmpFlt)

DIMENSION laBrFldFlt[2,ALEN(laBrTmpFlt,2)]
laBrFldFlt = ""
laBrFldFlt[2,1] = "ORDER"
laBrFldFlt[2,2] = lcBrowFlds
laBrFldFlt[2,3] = ""
laBrFldFlt[2,4] = ""
laBrFldFlt[2,5] = "laStylFle:1"

lcOrderVm = gfGenFlt('laStylFle',.T.,.T.)
DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]
=ACOPY(laBrTmpFlt,laBrFldFlt)

SELECT (lcAlias)

*--End of lfVOrder.