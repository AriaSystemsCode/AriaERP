*!********************************************************************
*: Program file  : SONAP50.PRG (Converted to 27 from NAP500.PRG)
*: Program desc. : PRINTING PACKING LIST LABELS.
*: For screen    : None.
*:         System: ARIA APPAREL SERIES
*:         Module: SO
*:      Developer: Adel Mohhamed El Gazzar (ADEL)
*:          DATE : 06/20/20001
*!********************************************************************
*! Refer to C(102341)
*!********************************************************************
*!B604903,1 AME 09/13/2001 fix the bug of wrong cartons numbering (in .lbx)
*!********************************************************************

*---- Program Variavles -----*
*-- Initializing the necessary variables
EXTERNAL ARRAY laData
STORE '' TO laData
lcMajDesc  = gfItemMask("HI")
lcScrMode = 'S'

lcLabToPrn = 'SONAP500'
STORE .F. TO llBrowse
llClose    = .T.
llFirst    = .T.
R_WIDTH    = 'N'
STORE " " TO lcWareName,lcWAddr1,lcWAddr2,lcWAddr3,lcPhone,lcStName,;
             lcStAddr1,lcStAddr2,lcStAddr3,lcPhone1,lcScFields
lcBrTit    = "Labels"
lcBrTitle  = "Order/Store"
lcPhonePic = gfPhoneTem()

*----   Program Settings -----*
*-- llMultWare     && Multi ware house.
llMultiWH = (gfGetMemvar('M_WareHouse') ='Y')
STORE ' '  TO lcTmpCurs,lcTmpStyle,TmpFile,lcOrdToPrn,lcScndIndx
IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  lcScFields = 'CORDER,CSTORE,CSHIPTO,NTOTCART,NFROM,NTO,CSTYCODE,CSKUNO,COLORCODE,CSIZE,NQUTY,nweight,CFLAG'
  SELECT NapLabel
  SCATTER FIELDS &lcScFields TO laData BLANK
  LaData[3]  = 'S'
  lcTmpCurs  = gfTempName()
  lcTmpStyle = gfTempName()
  TmpFile    = gfTempName()

  *--Adding the following variable to hold the temporary file which will hold the selected orders.
  lcOrdToPrn = gfTempName()
  lcScndIndx = gfTempName()
  CREATE TABLE (gcWorkDir+lcOrdToPrn);
               (cFlag C(1),cOrder C(6),cStore C(8),cShipTo C(1),nTotCart N(3),;
               nFrom N(3), nTo N(3), cSkuNo C(12), CSTYCODE C(12),;
               COLORCODE C(12), cSize C(12), NQUTY N(6), nweight N(6))
  INDEX ON cFlag+cOrder+cStore TAG(lcScndIndx) OF (lcOrdToPrn)
  INDEX ON cOrder+cStore TAG(lcOrdToPrn) OF (lcOrdToPrn)

  *-- This file to hold the range of labels that will be printed
  CREATE TABLE (gcWorkDir+TmpFile);
               (cOrder C(6),cStore C(8),cShipTo C(1), nTotCart N(3),;
               nFrom N(3), nTo N(3), cSkuNo C(12), CSTYCODE C(12),;
               COLORCODE C(12), cSize C(12), NQUTY N(6), nweight N(6))
  INDEX ON nFrom+nTo TAG(TmpFile) OF (TmpFile)
ENDIF

*-- Performing the screen
DO (gcScrDir + gcWinAppl +'\NAP500_0.SPX')

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To validate the selected order number.
*!*************************************************************
*! Example            : =lfvOrder()
*!*************************************************************
FUNCTION lfvOrder


PRIVATE lnAlias
lnAlias = SELECT()
IF !llBrowse .AND. EMPTY(laData[1])
  RETURN
ENDIF
SELECT ORDHDR
IF llBrowse .OR. !SEEK('O'+laData[1],'OrdHdr') 
  lcOrder = laData[1]
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = [Order:H="Order#",]+;
               [status:1:H="S",]+;
               [ACCOUNT:H="Acct",]+;
               [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
               [Customer.stname:30:H="Name",]+;
               [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
               [Open:H="Open.Qty.",]+;
               [OpenAmt:H="Open.Amt.",]+;
               [Ship:H="Ship.Qty.",]+;
               [Shipamt:H="Ship.Amt.",]+;
               [start:H="Start",]+;
               [Complete:H="Complete",]+;
               [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
               [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
               [Note1:6:H="Notes"]
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X"]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    laData[1] = laTemp[1]
  ELSE
    laData[1] = ''
  ENDIF
  laData[1] = IIF(EMPTY(laData[1]) AND lcOrder <> "?",lcOrder,laData[1])
ENDIF
llBrowse = .F.
IF EMPTY(laData[1])
  SELECT (lnAlias)
  _CUROBJ = OBJNUM(laData[1])
  RETURN
ENDIF
lcScrMode = 'E'
IF OrdHdr.Multi = "Y"
  =lfGetStore()
  SHOW GET laData[2] ENABLE
  SHOW GET ibStore ENABLE    
ELSE
  laData[2] = OrdHdr.Store
  =lfGetStyle()
  SHOW GET laData[2] DISABLE
  SHOW GET ibStore   DISABLE
ENDIF
IF !EMPTY(laData[1])
  IF OrdHdr.Multi = "Y" OR !EMPTY(OrdHdr.Store)
    SHOW GET LaData[3]  ENABLE
  ENDIF
  SHOW GET laData[4]    ENABLE
ELSE
  SHOW GET LaData[3]    DISABLE
  SHOW GET laData[4]    DISABLE
ENDIF
*SHOW GET pbClose,1 PROMPT "\<Cancel"
SHOW GET pbClose,1 PROMPT lcCancelbmp
llClose = .F.
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetStore
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To get the all the order's stores.
*!*************************************************************
*! Example            : =lfGetStore()
*!*************************************************************
FUNCTION lfGetStore

*-- To hold all the stores of the seleced order in case of multi-stores
CREATE CURSOR (lcTmpCurs);
             (cStore C(8),cStName C(30), cStAddr1 C(30),;
              cStAddr2 C(30), cStCity C(15), cStState C(3), cStZip C(10))
INDEX ON cStore TAG(lcTmpCurs) OF (lcTmpCurs)

SELECT OrdLine
=SEEK('O'+laData[1])
lcOrdStore = SPACE(8)
SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+laData[1]
  IF Store <> lcOrdStore
    =SEEK('S'+Account+Store,'Customer')
    INSERT INTO (lcTmpCurs);
          (cStore,cStName,cStAddr1,cStAddr2,cStCity,cStState,cStZip);
    VALUES(OrdLine.Store,Customer.StName,Customer.caddress1,Customer.caddress2,;
           SUBSTR(Customer.caddress3,1,15),SUBSTR(Customer.caddress4,1,3),SUBSTR(Customer.caddress5,1,10))
    lcOrdStore = Store
  ENDIF
ENDSCAN

*!*************************************************************
*! Name      : lfvStore
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To validate the store in case of multi-store order
*!*************************************************************
*! Example            : =lfvStore()
*!*************************************************************
FUNCTION lfvStore

lcOldOrd = laData[1]
lcOldDc  = laData[3]
IF llBrowse .OR. !SEEK(laData[2],lcTmpCurs)
  *-- To brows all the stores of the selected order
  =lfBrowStor()
ENDIF
laData[1] = lcOldOrd 
laData[3] = lcOldDc
SHOW GET laData[1]
SHOW GET laData[3]
IF EMPTY(laData[2])
  _CUROBJ = _CUROBJ
  RETURN(.F.)
ENDIF
llBrowse = .F.

*-- lfGetStyle() function is used to get all the styles of the 
*-- selected store.
=IIF(!EMPTY(laData[2]),lfGetStyle(),.F.)

*!*************************************************************
*! Name      : lfBrowStor
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To brows all the stores for the selected order.
*!*************************************************************
*! Example            : =lfBrowStor()
*!*************************************************************
FUNCTION lfBrowStor

SELECT (lcTmpCurs)

lcBrFields = [cStore:8:H="Store",cStName:30:H="Name",cStAddr1:30:H="Address1",]+;
             [cStAddr2:30:H="Address2",cStCity:15:H="City",]+;
             [cStState:3:H="State",cStZip:10:H="Zip"]


laData[2] = IIF(ARIABROW('',"Stores",gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,;
         gnBrFSCol2),&lcTmpCurs..cStore,SPACE(8))

SHOW GET laData[2]

*!*************************************************************
*! Name      : lfGetStyle
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To get the all the styles within the store
*!*************************************************************
*! Example            : =lfGetStyle()
*!*************************************************************
FUNCTION lfGetStyle

*-- To hold the all the styles of the selected store.
CREATE CURSOR (lcTmpStyle);
             (CSTYCODE C(19),cStyDesc C(30),cSeason C(2),;
              cGroup C(2),cDivision C(2),nPriceA N(5,2),nTotStk N(7),;
              nTotWip N(7), nTotOrd N(7))
INDEX ON CSTYCODE TAG(lcTmpStyle) OF (lcTmpStyle)
SELECT OrdLine
=SEEK('O'+laData[1]+laData[2])
SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = 'O'+laData[1] + laData[2] 
    =SEEK(Style,'Style')
    INSERT INTO (lcTmpStyle);
         (CSTYCODE,cStyDesc,cSeason,cGroup,cDivision,nPriceA,;
         nTotStk,nTotWip,nTotOrd);
    VALUES(OrdLine.Style,Style.Desc,Style.Season,;
           Style.cStyGroup,Style.cDivision,Style.PriceA,Style.TotStk,;
           Style.TotWip,Style.TotOrd)
ENDSCAN

*!*************************************************************
*! Name      : lfBrowSty
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To brows all the styles for the selected store
*!*************************************************************
*! Example            : =lfBrowSty()
*!*************************************************************
FUNCTION lfBrowSty

SELECT (lcTmpStyle)
lcBrFields = [CSTYCODE:25:H=lcMajDesc,cStyDesc:20:H="Description",]+;
             [cSeason:5:H="Season",cDivision:5:H="Division",]+;
             [cGroup:5:H="Group",nPriceA:H="PriceA",nTotStk:7:H="TotStk",]+;
             [nTotWip:7:H="TotWip",nTotOrd:7:H="TotOrd"]
LOCATE
laData[7] = IIF(ARIABROW('',"Style/Color",gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,;
         gnBrFSCol2),&lcTmpStyle..CSTYCODE,SPACE(12))
LOCATE
SHOW GET laData[7]

*!*************************************************************
*! Name      : lfBrows
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To brows the selected of Order.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows

PRIVATE lnAlias

lnAlias = SELECT()

SELECT (TmpFile)
GOTO TOP
BROWSE FIELDS ;
     nFrom      :H = 'From'         :R,;
     nTo        :H = 'To'           :R,;
     CSTYCODE     :H = 'Style Number' :R,; 
     cSkuNo     :H = 'Style SKU'    :R,;
     COLORCODE     :H = 'Color'        :R,; 
     cSize      :H = 'Size'         :R,; 
     nweight    :H = 'Weight'       :R,; 
     NQUTY       :H = 'Qty.'         :R; 
     WINDOW    NAP5001  ;
     IN WINDOW (gcBaseWind);
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
SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To be proceesed when moving into the browse
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

laData[5]   = &TmpFile..nFrom
laData[6]   = &TmpFile..nTo
laData[7]   = &TmpFile..CSTYCODE
laData[8]   = &TmpFile..cSkuNo
LaData[9]   = &TmpFile..COLORCODE
laData[10]  = &TmpFile..cSize
laData[11]  = &TmpFile..NQUTY
laData[12]  = &TmpFile..nweight

SHOW GET laData[5]
SHOW GET laData[6]
SHOW GET laData[7]
SHOW GET laData[8]
SHOW GET laData[9]
SHOW GET laData[10]
SHOW GET laData[12]
SHOW GET laData[11]
IF !EOF(TmpFile)
  SHOW GET pbRemove ENABLE
  SHOW GET pbModify ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvShipTo
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the ship to field
*!*************************************************************
*! Example            :  lfvShipTo()
*!*************************************************************
FUNCTION lfvShipTo

IF !(LaData[3] $ "DS")
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"You have to enter S for ship to address"+;
                                           " or D for distribution center address.")
  _CUROBJ = _CUROBJ                
ENDIF

*!*************************************************************
*! Name      : lfvTotCart
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the Total # of Cartons field.
*!*************************************************************
*! Example            :  lfvTotCart()
*!*************************************************************
FUNCTION lfvTotCart

IF laData[4] < 1
  RETURN(.F.)
ENDIF

laData[6]   = laData[4]
laData[5] = 1
SHOW GET laData[6]
SHOW GET laData[5]
SHOW GET pbNew   ENABLE
SHOW GET pbPrint ENABLE
_CUROBJ = OBJNUM(pbNew)

*!*************************************************************
*! Name      : lvNew
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the New push button
*!*************************************************************
*! Example            :  lfvNew()
*!*************************************************************
FUNCTION lfvNew

SHOW GETS WINDOW "NAP5003" DISABLE
SHOW GET ibTabs ENABLE
STORE SPACE(12) TO laData[8],laData[7],LaData[9],laData[10]
laData[11] = 0
laData[12] = 0
SELECT (TmpFile)
GOTO BOTTOM
laData[5] = 1
laData[6] = 10
IF !llFirst AND nTo <> laData[4]
  laData[5] =  &TmpFile..nTo  + 1
ENDIF
llFirst = .F.
laData[6] = laData[4]
SHOW GET laData[5]  DISABLE
SHOW GET laData[6]  ENABLE
SHOW GET laData[7]  ENABLE
SHOW GET ibStyle    ENABLE
SHOW GET laData[8]  ENABLE
SHOW GET laData[9]  ENABLE
SHOW GET laData[10] ENABLE
SHOW GET laData[12] ENABLE
SHOW GET laData[11] ENABLE
SHOW GET pbClose    ENABLE
SHOW GET laData[5]  DISABLE
SHOW GET pbSave     ENABLE
SELECT (TmpFile)
APPEND BLANK
REPLACE cOrder   WITH laData[1],;
        cStore   WITH laData[2]  ,;
        cShipTo  WITH LaData[3]    ,;
        nTotCart WITH laData[4]
SHOW WINDOW (lcBrTit) REFRESH SAME
_CUROBJ = OBJNUM(laData[6])

*!*************************************************************
*! Name      : lfvModify
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the modify push button
*!*************************************************************
FUNCTION lfvModify

SHOW GETS WINDOW "NAP5002" ENABLE
_CUROBJ = OBJNUM(laData[5])

*!*************************************************************
*! Name      : lfvRemove
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the Remove push button
*!*************************************************************
FUNCTION lfvRemove
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (TmpFile)
DELETE
=lfwBrow()
SHOW WINDOW (lcBrTit) REFRESH SAME
SHOW GET pbRemove DISABLE
SHOW GET pbModify DISABLE
laData[12] = 0
STORE 0 TO laData[5],laData[6],laData[11]
STORE SPACE(12) TO laData[8],laData[7],laData[9],laData[10]
LaData[3]   = 'S'
=lfShowGets()
_CUROBJ = OBJNUM(pbNew)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvPrint
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To validate the Print Push Button
*!*************************************************************
*! Example            : =lfvPrint()
*!*************************************************************
FUNCTION lfvPrint

PRIVATE lnCounter

SELECT NapLabel
GOTO TOP
IF EOF('NapLabel')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No records selected. Cannot Print.")
  _CUROBJ = OBJNUM(laData[1])
  RETURN
ENDIF
SELECT (lcOrdToPrn)
SET ORDER TO &lcOrdToPrn
SELECT NapLabel
SCAN
  IF !SEEK(cOrder+cStore,lcOrdToPrn)
    WAIT WINDOW "Order/Store :"+cOrder+"/"+cStore NOWAIT
    SCATTER MEMVAR MEMO
    INSERT INTO (lcOrdToPrn) FROM MEMVAR
  ENDIF
ENDSCAN
WAIT CLEAR
SET ORDER TO &lcScndIndx IN &lcOrdToPrn
lcStatus  = IIF(SEEK(' ',lcOrdToPrn),"ENABLE","DISABLE")
lcStatus1 = IIF(SEEK('û',lcOrdToPrn),"ENABLE","DISABLE")
SET ORDER TO &lcOrdToPrn IN &lcOrdToPrn

DO (gcScrDir + gcWinAppl +'\NAP500A.SPX')
IF lcScrMode = 'S'
  _CUROBJ = OBJNUM(laData[1])
ENDIF  

*!*************************************************************
*! Name      : lfGetWareh
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To get the warehouse information.
*!*************************************************************
*! Example            :  lfGetWareh()
*!*************************************************************
*B801491,1 KHM 02/15/98 Added
*!*************************************************************
FUNCTION lfGetWareh

*-- To get the warehouse address.
IF llMultiWH 
  IF SEEK(OrdHdr.cWareCode,'WareHous')
    lcWareName  = WareHous.cDesc
    lcWAddr1    = WareHous.cAddress1
    lcWAddr2    = WareHous.cAddress2
    lcWAddr3    = ALLTRIM(SUBSTR(WareHous.cAddress3,1,15))+' '+ALLTRIM(SUBSTR(WareHous.cAddress4,1,3));
                  +' '+ALLTRIM(SUBSTR(WareHous.cAddress5,1,10))
    lcPhone     = WareHous.cPhone
  ENDIF 
ELSE
  GO TOP IN WAREHOUS
    lcWareName  = WareHous.cDesc
    lcWAddr1    = WareHous.cAddress1
    lcWAddr2    = WareHous.cAddress2
    lcWAddr3    = ALLTRIM(SUBSTR(WareHous.cAddress3,1,15))+' '+ALLTRIM(SUBSTR(WareHous.cAddress4,1,3));
                  +' '+ALLTRIM(SUBSTR(WareHous.cAddress5,1,10))
    lcPhone     = WareHous.cPhone
ENDIF
IF EMPTY(lcWAddr2)
  lcWAddr2 = lcWAddr3
  lcWAddr3 = lcPhone
  lcPhone  = ''
ENDIF 

*!*************************************************************
*! Name      : lfvAccAddr
*! Developer : Khalid Mohi El-Din
*! Date      : 05/13/1998
*! Purpose   : To get the account address.
*!*************************************************************
*! Example            : =lfvAccAddr()
*!*************************************************************
FUNCTION lfvAccAddr
            
lcStName  = LEFT(Customer.StName,24)
lcStAddr1 = LEFT(Customer.caddress1,27)
lcStAddr2 = IIF(!EMPTY(Customer.caddress2),LEFT(Customer.caddress2,27),;
            ALLTRIM(SUBSTR(Customer.caddress3,1,15))+' '+ALLTRIM(SUBSTR(Customer.caddress4,1,3))+;
            ' '+ALLTRIM(SUBSTR(Customer.caddress5,1,10)))
lcStAddr3 = IIF(!EMPTY(Customer.caddress2),ALLTRIM(SUBSTR(Customer.caddress3,1,15))+' '+ALLTRIM(SUBSTR(Customer.caddress4,1,3))+;
                ' '+ALLTRIM(SUBSTR(Customer.caddress5,1,10)),'')
lcPhone1  = Customer.Phone1

*!*************************************************************
*! Name      : lfvFrom
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the from field
*!*************************************************************
*! Example            :  lfvFrom()
*!*************************************************************
FUNCTION lfvFrom

IF laData[5] < 1
  RETURN(.F.)
ENDIF

IF laData[5] > laData[4]
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"The range should not be greater than "+STR(laData[4],3)+".")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvTo
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the To field
*!*************************************************************
*! Example            :  lfvTo()
*!*************************************************************
FUNCTION lfvTo

IF laData[5] > laData[6]
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Label range error. Please retry.")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF laData[6] > laData[4]
  =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"The range should not be greater than "+STR(laData[4],3)+".")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SELECT (TmpFile)
REPLACE nFrom WITH laData[5] ,;
        nTo   WITH laData[6]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvStyle
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the style field.
*!*************************************************************
*! Example            :  lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF llBrowse .OR. (!EMPTY(laData[7]) AND !SEEK(laData[7],lcTmpStyle))
  =lfBrowSty()
ENDIF
llBrowse = .F.
SELECT (TmpFile)
REPLACE CSTYCODE WITH laData[7]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvSkuNo
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the SKU No field.
*!*************************************************************
*! Example            :  lfvSkuNo()
*!*************************************************************
FUNCTION lfvSkuNo
SELECT (TmpFile)
REPLACE cSkuNo WITH laData[8]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvColor
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the color field.
*!*************************************************************
*! Example            :  lfvColor()
*!*************************************************************
FUNCTION lfvColor

SELECT (TmpFile)
REPLACE COLORCODE WITH LaData[9]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvWeight
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the weight field.
*!*************************************************************
*! Example            :  lfvWeight()
*!*************************************************************
FUNCTION lfvWeight
SELECT (TmpFile)
REPLACE nweight WITH laData[12]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvSize
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the Size field.
*!*************************************************************
*! Example            :  lfvSize()
*!*************************************************************
FUNCTION lfvSize
SELECT (TmpFile)
REPLACE cSize WITH laData[10]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvQty
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To validate the Qty field.
*!*************************************************************
*! Example            :  lfvQty()
*!*************************************************************
FUNCTION lfvQty
SELECT (TmpFile)
REPLACE NQUTY WITH laData[11]
SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvClose
*! Developer : Khalid Mohi El-Din
*! Date      : 12/21/1997
*! Purpose   : Valid function for Push Button pbClose.
*!*************************************************************
*! Example            : =lfvClose()
*!*************************************************************
FUNCTION lfvClose

DO lpClose

PROCEDURE lpClose

IF !llClose
  IF gfModalGen('QRM00000B00006',.F.,.F.,.F.," Are you sure you want to cancel this order?") = 1
    SELECT (TmpFile)
    ZAP
    *-- To rebrows the file
    =lfBrows()
    STORE SPACE(6)  TO laData[1]
    STORE SPACE(8)  TO laData[2]
    STORE 0         TO laData[4],laData[5],laData[6],laData[11],laData[12]
    STORE SPACE(12) TO laData[8],laData[7],LaData[9],laData[10]
    LaData[3]       = 'S'
    =lfShowGets()
    llClose = .T.
    SHOW GET pbClose,1 PROMPT lcClosebmp
    SHOW GET ibOrderNo ENABLE
    SHOW GET laData[1] ENABLE
    SHOW GET pbNew     DISABLE
    SHOW GET pbSave    DISABLE
    ACTIVATE WINDOW "NAP5003"
    _CUROBJ = OBJNUM(laData[1])
  ELSE
    _CUROBJ = _CUROBJ
  ENDIF  
ELSE
  glQuitting = .T.
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfShowGets
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To Reinitialize the variables and the stuts of the fields
*!*************************************************************
*! Example            :  lfShowGets()
*!*************************************************************
FUNCTION lfShowGets

SHOW GET ibStore    DISABLE
SHOW GET laData[2]  DISABLE
SHOW GET LaData[3]  DISABLE
SHOW GET laData[4]  DISABLE
SHOW GET laData[6]  DISABLE
SHOW GET laData[5]  DISABLE
SHOW GET laData[7]  DISABLE
SHOW GET ibStyle    DISABLE
SHOW GET laData[8]  DISABLE
SHOW GET LaData[9]  DISABLE
SHOW GET laData[10] DISABLE
SHOW GET laData[12] DISABLE
SHOW GET laData[11] DISABLE


*!*************************************************************
*! Name      : lfChkRange
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To check the integrity of the carton's ranges.
*!*************************************************************
*! Example            :  lfChkRange()
*!*************************************************************
FUNCTION lfChkRange

STORE .T. TO llRet,llFirst
SELECT (TmpFile)
GOTO TOP
lnToRange = nTo

SCAN
  WAIT WINDOW "Checking the integrity of label's ranges" NOWAIT  
  IF !llFirst
    IF lnToRange + 1 <> nFrom OR nFrom > nTo
      =gfDialog("I","There is an error in the label's ranges."+;
                    " Please check the integrity of the labels")
      llRet = .F.
      EXIT              
    ENDIF
    lnToRange = nTo    
  ELSE
    IF nFrom <> 1
      =gfDialog("I","There is an error in the label's ranges."+;
                    " Please check the integrity of the labels")
      llRet = .F.
      EXIT              
    ENDIF    
  ENDIF
  llFirst = .F. 
ENDSCAN
WAIT CLEAR
RETURN(llRet)

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : Valid function for the browsing button (ibOrder,ibStore)
*!*************************************************************
*! Example            :  lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName
llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC DO lpClose
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 


*!*************************************************************
*! Name      : lfvDeact
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : The READ Deactivate function of screen NAP500
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfvDeact

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'NAP5002', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'NAP5002', 'laData[6]'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'NAP5003', IIF(lcScrMode= 'S','laData[1]','laData[4]')
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/13/1998
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF


*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate Select button.
*!*************************************************************
*! Example            :  lfvSelect()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfOrdBrows
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcOrdToPrn)
GOTO TOP
IF _WINDOWS
  BROWSE FIELDS ;
         cFlag    :H = ' '                :R,;
         cOrder   :H = 'Order '           :R,;
         cStore   :H = 'Store  '          :R,;
         cShipTo  :H = 'Ship to Store/DC' :R,;
         nTotCart :H = 'Labels  '         :R;
         WINDOW    NAP500B ;
         IN WINDOW NAP500A ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         NOEDIT            ;
         LOCK 0            ;
         FONT "FOXFONT",9  ;
         WHEN lfwOrdBrow() ;
         TITLE lcBrTitle
ELSE
  BROWSE FIELDS ;
         cFlag    :H = ' '                :2:R,;
         cOrder   :H = 'Order '           :12:R,;
         cStore   :H = 'Store  '          :12:R,;
         cShipTo  :H = 'Ship to store/DC' :18:R,;
         nTotCart :H = 'Labels        '   :15:R;
         WINDOW    NAP500B ;
         IN WINDOW NAP500A ;
         NOMENU            ;         
         NOAPPEND          ;
         NODELETE          ;         
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         NOEDIT            ;
         LOCK 0            ;
         COLOR SCHEME 10   ;
         WHEN lfwOrdBrow() ;
         TITLE lcBrTitle
ENDIF       
SHOW WINDOW (lcBrTitle) REFRESH SAME
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwOrdBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 12/27/1998
*! Purpose   : To be done whenever the pointer of the browse changed.
*!*************************************************************
*! Example            :  lfwOrdBrow()
*!*************************************************************
FUNCTION lfwOrdBrow

IF !EMPTY(&lcOrdToPrn..cFlag)
  SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
ELSE
  SHOW GET pbSelect,1 PROMPT '\<Select' 
ENDIF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate Select button.
*!*************************************************************
*! Example            :  lfvSelect()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvSelect
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT (lcOrdToPrn)

REPLACE cFlag WITH IIF(EMPTY(cFlag),'û',' ')
IF EMPTY(cFlag)
  SHOW GET pbSelect,1 PROMPT '\<Select' 
ELSE
  SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
ENDIF

SET ORDER TO &lcScndIndx
IF SEEK(' ')
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF
IF SEEK('û')
  SHOW GET pbSelNon ENABLE
ELSE
  SHOW GET pbSelNon DISABLE
ENDIF

SET ORDER TO &lcOrdToPrn
SEEK ""
SHOW WINDOW (lcBrTitle) REFRESH SAME

SELECT(lnAlias)


*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate Select All button.
*!*************************************************************
*! Example            :  lfvSelAll()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvSelAll
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcOrdToPrn)
REPLACE ALL cFLAG WITH 'û'
SEEK ""
SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
SHOW GET pbSelAll DISABLE
SHOW GET pbSelNon ENABLE

SHOW WINDOW (lcBrTitle) REFRESH SAME
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfSelNon
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate Select Non button.
*!*************************************************************
*! Example            :  lfvSelNon()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvSelNon
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcOrdToPrn)
REPLACE ALL cFLAG WITH SPACE(1)
SEEK ""
SHOW GET pbSelect,1 PROMPT '\<Select' 
SHOW GET pbSelAll ENABLE
SHOW GET pbSelNon DISABLE

SHOW WINDOW (lcBrTitle) REFRESH SAME

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate Invert button.
*!*************************************************************
*! Example            :  lfvInvert()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvInvert
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcOrdToPrn)
REPLACE ALL cFLAG WITH IIF(EMPTY(cFlag),'û',SPACE(1))

SET ORDER TO &lcScndIndx
IF SEEK(' ')
  SHOW GET pbSelAll ENABLE
ELSE
  SHOW GET pbSelAll DISABLE
ENDIF
IF SEEK('û')
  SHOW GET pbSelNon ENABLE
ELSE
  SHOW GET pbSelNon DISABLE
ENDIF

SET ORDER TO &lcOrdToPrn
SEEK ""

IF EMPTY(cFlag)
  SHOW GET pbSelect,1 PROMPT '\<Select' 
ELSE
  SHOW GET pbSelect,1 PROMPT '\<UnSelect' 
ENDIF
SHOW WINDOW (lcBrTitle) REFRESH SAME

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvSave
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To save the selected lables in the customized master file.
*!*************************************************************
*! Example            :  lfvSave()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvSave

*-- This function is used to check the integrity of the ranges
IF !lfChkRange()
  _CUROBJ = OBJNUM(pbNew)
  RETURN
ENDIF

SELECT (TmpFile)
SCAN
  WAIT WINDOW "Updating the master file" NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO NapLabel FROM MEMVAR
ENDSCAN
WAIT CLEAR

SELECT (TmpFile)
ZAP

*-- To rebrows the file
=lfBrows()
STORE SPACE(6)  TO laData[1]
STORE SPACE(8)  TO laData[2]
STORE 0         TO laData[4],laData[5],laData[6],laData[11],laData[12]
STORE SPACE(12) TO laData[8],laData[7],LaData[9],laData[10]
LaData[3]       = 'S'
=lfShowGets()
llClose = .T.
*SHOW GET pbClose,1 PROMPT "\<Close"
SHOW GET pbClose,1 PROMPT lcClosebmp

SHOW GET ibOrderNo ENABLE
SHOW GET laData[1] ENABLE
SHOW GET pbNew     DISABLE
SHOW GET pbModify  DISABLE
SHOW GET pbRemove  DISABLE
SHOW GET pbSave    DISABLE
_CUROBJ = OBJNUM(laData[1])

*!*************************************************************
*! Name      : lfvPrClose
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate the close button in the second screen.
*!*************************************************************
*! Example            :  lfvPrClose()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfvPrClose
PRIVATE llPrint

llPrint = .F.
*-- Setting the index to lcScndIndx in the lcOrdToPrn file in order to print
*-- the selected orders/stores only.
SELECT (lcOrdToPrn)
SET ORDER TO &lcScndIndx
IF !SEEK('û')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No records selected. Cannot Print.")
  CLEAR READ
  _CUROBJ = OBJNUM(laData[1])  
  RETURN
ENDIF
CLEAR READ
S_CLOSEALL=.F.
lcOGWinTit = 'Report of Labels'
*--Would you like to preview/print the purchase orders just created?
*--<Preview>  <Print>  <None>.
lnSelDev = gfModalGen('QRM00000B36010',.F.,.F.,.F.," Would you like to preview/print the selected items?")
IF lnSelDev <> 3
  IF lnSelDev = 1
    gcDevice = 'SCREEN'
  ELSE
    IF ! pSetup(.T.,.F.)
      RETURN
    ENDIF  
  ENDIF
  *-- Scanning for the selected orders/stores and print them
  llPrint = .T.
  *--Start the report printing.
  SET DEVICE TO PRINT
  *--Set to maupolate the printing.
  lcPdSETup = _pdsetup
  DO CASE
    CASE gcDevice = 'SCREEN'
      _PDSETUP=''
      gcOutFile = gcWorkDir+gfTempName()+'.TXT'
      SET PRINTER TO &gcOutFile
    CASE gcDevice = 'FILE'  
      _PDSETUP=''  
      SET PRINTER TO &gcOutFile  
    CASE gcDevice = 'PRINTER'  
      _PDSETUP=''  
     gcOutFile = gcWorkDir+gfTempName()+'.TXT'
     SET PRINTER TO &gcOutFile
  ENDCASE
  SELECT (lcOrdToPrn)
  SCAN REST WHILE cFlag+cOrder+cStore = 'û'
    IF !WEXIST('lcTemp')
      DEFINE WINDOW lcTemp FROM 1,1 TO 2,2
    ENDIF
    ACTIVATE WINDOW lcTemp NOSHOW
    =SEEK('O'+&lcOrdToPrn..cOrder,'OrdHdr')
    =SEEK('O'+&lcOrdToPrn..cOrder+&lcOrdToPrn..cStore,'OrdLine')
    =lfGetWareh()
    =IIF(EMPTY(&lcOrdToPrn..cStore),SEEK("M"+OrdHdr.Account,'Customer'),;
                       SEEK('S'+OrdHdr.Account+&lcOrdToPrn..cStore,'Customer'))                       
    IF &lcOrdToPrn..cShipTo = 'D'
      IF !EMPTY(Customer.Dist_Ctr)
         =SEEK('S'+OrdHdr.Account+Customer.Dist_Ctr,'Customer')
      ELSE
        = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"There is no DC for store "+ALLTRIM(cStore)+;
                      ". The store address will be printed.")
      ENDIF                
    ENDIF   
    =lfvAccAddr() 
    SELECT NapLabel
    =SEEK(&lcOrdToPrn..cOrder+&lcOrdToPrn..cStore)
    SCAN REST WHILE cOrder+cStore+STR(nFrom,3)+STR(nTo,3) = ;
                    &lcOrdToPrn..cOrder+&lcOrdToPrn..cStore
      WAIT WINDOW "Printing labels from: "+STR(nFrom,3)+"To : "+STR(nTo,3) NOWAIT
      lnCount = nFrom
      FOR lnCounter = 1 TO (nTo - nFrom) + 1
         SET CONSOLE OFF            
         LABEL FORM (gcRepHome+gcWinAppl +'\'+lcLabToPrn) NEXT 1 TO PRINT noconsole
         SET CONSOLE ON
         lnCount = lnCount + 1
      ENDFOR
    ENDSCAN
    SELECT(lcOrdToPrn)
  ENDSCAN  
  WAIT CLEAR
  DO ENDREPORT
ENDIF
SET DEVICE TO SCREEN
IF WEXIST('lcTemp')
  DEACTIVATE WINDOW lcTemp
ENDIF
*-- To reinitialize the variables and the status of the get fields
STORE SPACE(6)  TO laData[1]
STORE SPACE(8)  TO laData[2]
STORE 0         TO laData[4],laData[5],laData[6],laData[11],laData[12]
STORE SPACE(12) TO laData[8],laData[7],LaData[9],laData[10]
LaData[3] = 'S'
=lfShowGets()
SHOW GET ibOrderNo ENABLE
SHOW GET laData[1] ENABLE
SHOW GET pbNew     DISABLE
SHOW GET pbSelNon  DISABLE
SHOW GET pbClose,1 PROMPT "\<Close"
llClose = .T.
=lfRefresh()
_CUROBJ = OBJNUM(laData[1])

*-- Deleting the records from the customized master file.
IF llPrint
  SELECT (lcOrdToPrn)
  IF SEEK('û')
    SCAN REST WHILE cFlag+cOrder+cStore = 'û'
      WAIT WINDOW "Deleting Order/Store : "+cOrder+"/"+cStore NOWAIT
      SELECT NapLabel
      IF SEEK(&lcOrdToPrn..cOrder+&lcOrdToPrn..cStore)
        DELETE REST WHILE cOrder+cStore+STR(nFrom,3)+STR(nTo,3) = ;
                        &lcOrdToPrn..cOrder+&lcOrdToPrn..cStore
      ENDIF                
    ENDSCAN
  ENDIF
  WAIT CLEAR
  SELECT (lcOrdToPrn)
  SEEK('û')
  DELETE REST WHILE cFlag+cOrder+cStore = 'û'
ENDIF

*!*************************************************************
*! Name      : lfDeactiv
*! Developer : Khalid Mhoi El-Din
*! Date      : 12/27/1998
*! Purpose   : To validate the deactivation of the screen.
*!*************************************************************
*! Example            :  lfDeactiv()
*!*************************************************************
*C101394,1 KHM 12/27/98 Added
*!*************************************************************
FUNCTION lfDeactiv

IF WONTOP() = lcBrTitle
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'NAP500C', 'PbPrnClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'NAP500C', 'pbSelect'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'NAP500C', 'PbPrnClose'
ENDIF
RETURN .F.

FUNCTION lfshow

IF lcScrMode = 'S'
  _CUROBJ  = OBJNUM(laData[1])
ENDIF  
