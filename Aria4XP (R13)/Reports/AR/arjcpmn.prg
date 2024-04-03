*:***************************************************************************
*: Program file  : ARJCPMN
*: Program desc. : Shipping Manifest Report For JCPENNEY
*: System        : Aria4XP
*: Module        : Account Receivable (AR)
*: Developer     : TAREK MOHAMMED IBRAHIM - TMI
*: Date          : 03/16/2011
*: Reference     : E302874,1  (a4) [T20101213.0032]
*:               : E302875,1 (A27)
* Note : this report was a copy of the original one in A27 updated to meet the new requiremnts of A4xp
*:***************************************************************************
#INCLUDE R:\Aria4xp\reports\ar\arjcpmn.H
*- if no accuont is selected in the standard case 'FORM A' then do not run the report
IF EMPTY(lcRpAcct)
  =gfModalGen('INM02071B00000','ALERT')   && No account selected to proceed.
  RETURN
ENDIF

IF lcRpName = 'ARJCPMN'
  IF lcRpPrint = 'I' AND EMPTY(lcRpDistCt)
    =gfModalGen('INM52030B00000','ALERT')   && No store was selected.
    RETURN
  ENDIF
ENDIF

*-- If user changed filter from OG
IF loOGScroll.llOGFltCh
  PRIVATE lcMasterFl
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile)
    IF RECCOUNT(lcWorkFile) > 0
      SELECT (lcWorkFile)
      ZAP
    ENDIF
  ELSE
    DO lpCreatFil
  ENDIF

  *- IF Form 'A' is selected ( the standard case )
  IF lcRpName = 'ARJCPMN'

    *-- Case Print by BOL
    IF lcRpPrint = 'B'
      lcMasterFl = 'Pack_Hdr'
      SELECT Pack_Hdr
      SET RELATION TO Account + Bill_ladg INTO BOL_Hdr
      DO lpCollData WITH 'B'

      SELECT Pack_Hdr
      SET RELATION TO     && break relation after collecting Data
    ELSE  && Else Print by Invoice
      lcMasterFl = 'InvHdr'
      SELECT InvHdr
      DO lpCollData WITH 'I'
    ENDIF

  ELSE

    *- FORM 'B' is selected , the print by BOL
    DO lpCollData

  ENDIF

ENDIF
*-- EndIf of user changed filter from OG [End.]
*-- If no records in temp file (empty)
SELECT (lcWorkFile)
LOCATE
*-- If Seek is successful (There's Records)
IF !EOF()
  DO gfDispRe WITH EVALUATE('lcRpName')
ELSE     && there is no records in Temp file
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
*-- EndIf of no records in temp file (empty)

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Collect data for the temporary file
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PARAMETERS lcPrintTyp
IF lcRpName = 'ARJCPMN' AND TYPE('lcPrintTyp') $ 'UL'
  RETURN
ENDIF

PRIVATE lcWareCode
*-- Initializing memory variables used in the Temp File
STORE '' TO m.cWareDesc,m.cWareAddr,m.cWareZip,m.cCusVend,;
            m.cRoutePEPS,m.cStoreNo,m.cStoreLoc,m.cCustPo,;
            m.cDC_Name,m.cDC_Addr1,m.cDC_Addr2,cDC_Addr3,m.cGroupKey
STORE 0 TO m.nCartons,m.nWeight

*-- Collect data according to Type of Print By [Begin.]

IF lcRpName = 'ARJCPMN'

  SET ORDER TO (lcWorkFile) IN (lcWorkFile)

  *-- If Print by Bill of Lading
  IF lcPrintTyp = 'B'
    lcRpExp = [Account + Pack_no = lcRpAcct AND ] + lcRpExp + ;
              [ AND !EOF('BOL_Hdr')]

    PRIVATE lcBillNo
    GO TOP
    *-- Scan loop on Master file (Pack_Hdr) [Begin.]
    SCAN FOR &lcRpExp WHILE INKEY() <> 32
      lcWareCode   = BOL_Hdr.W_Code
      * Get BOL Carrier from the new filed ShipVia in the BOLHDR file
      m.cRoutePEPS = IIF(EMPTY(BOL_Hdr.Carrier) AND !EMPTY(BOL_Hdr.ShipVia),gfCodDes(BOL_Hdr.ShipVia,'SHIPVIA'),BOL_Hdr.Carrier)

      m.cGroupKey  = lcRpAcct + Pack_Hdr.Bill_Ladg

      *-- If (Bill of lading) Store Distr. Center will be BOL_Hdr Store
      lcRpDistCt = BOL_Hdr.Store

      m.nCartons  = Tot_Cart
      m.nWeight   = Tot_Wght
      m.cCustPo   = IIF(SEEK('O'+Pack_hdr.Order,'OrdHdr'),OrdHdr.CustPo,'')
      m.cStoreNo  = Store
      m.cStoreLoc = IIF(SEEK('S'+lcRpAcct+m.cStoreNo,'Customer'),;
                    IIF(EMPTY(Customer.cAddress3),ALLTRIM(Customer.cAddress4),;
                    ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)),'')

      DO lpInsRecs      && Insert record into the temporary file
    ENDSCAN
    *-- Scan loop on Master file (Pack_Hdr) [End.]

  ELSE    *-- Else Print by Invoice

    lcRpExp = [Account + Invoice = lcRpAcct AND ;
              InvHdr.Status <> "V" AND ] + lcRpExp

    PRIVATE lcShipVia
    GO TOP
    *-- Scan loop on Master file (InvHdr) [Begin.]

    SCAN FOR &lcRpExp WHILE INKEY() <> 32
      lcShipVia    = ShipVia
      lcWareCode   = cWareCode
      m.cRoutePEPS = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
      m.cGroupKey  = lcRpAcct+lcWareCode+lcShipVia

      m.nCartons  = Cartons
      m.nWeight   = Weight
      m.cCustPo   = CustPo
      m.cStoreNo  = Store
      m.cStoreLoc = IIF(SEEK('S'+lcRpAcct+m.cStoreNo,'Customer'),;
                    IIF(EMPTY(Customer.cAddress3),ALLTRIM(Customer.cAddress4),;
                    ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)),'')

      DO lpInsRecs      && Insert record into the temporary file
    ENDSCAN
  ENDIF

ELSE

  lcMasterFl = 'Pack_LIN'
  SELECT PACK_HDR
  lcPckHdrOrd = ORDER()
  SET ORDER TO ACCPACK             && key :ACCOUNT+PACK_NO
  SET RELATION TO Account + Bill_ladg INTO BOL_Hdr
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE

  =gfSEEK('M'+lcRpAcct,'Customer')
  m.BtName = CUSTOMER.BtName

  SELECT PACK_HDR
  =gfSeek(lcRpAcct,'PACK_HDR')

  SET ORDER TO BYBOL IN (lcWorkFile)

  SCAN REST WHILE ACCOUNT+PACK_NO = lcRpAcct FOR &lcRpExp
  &&17-digit BOL# : (the same logic used in the VICS standard BOL form "B")
    m.BOL_NO =   PADL(ALLTRIM(lcXMANUFID) , 7 , '0') + PADL(ALLTRIM(PACK_HDR.BILL_LADG) , 9 , '0')
    m.BOL_NO = m.BOL_NO + lfGetCheckDigit(m.BOL_NO,'E')
    m.DC = BOL_HDR.STORE
    m.CPUA = BOL_HDR.CPUA
    m.SHIP_DATE = BOL_HDR.SHIP_DATE
    m.cCustPo = ORDHDR.CUSTPO
    m.ORDER = ORDHDR.ORDER
    m.DEPT = ORDHDR.DEPT
    m.cStoreNo = PACK_HDR.STORE
    m.PACK_NO = PACK_HDR.PACK_NO
    m.CARRIERCOD = BOL_HDR.CARRIERCOD
    m.CARRIER = gfCodDes(BOL_Hdr.ShipVia,'SHIPVIA')

    m.PCS = 0
    m.nCartons = 0
    SELECT PACK_LIN
    =gfSeek(PACK_HDR.PACK_NO,'PACK_LIN')
    SCAN REST WHILE PACK_NO+STR(LINE_NO,6)+STYLE+CPACKCOLOR = PACK_HDR.PACK_NO   && INDEX : PACK_LIN
      m.STYLE = SUBSTR(PACK_LIN.STYLE,1,lnMajor )
      IF !SEEK(m.BOL_NO+m.PACK_NO+m.DC+m.CPUA+m.cCustPo+m.ORDER+m.DEPT+m.cStoreNo+m.STYLE,lcWorkFile)
        INSERT INTO (lcWorkFile) FROM MEMVAR
      ENDIF
    ENDSCAN

    SELECT (lcWorkFile)
    REPLACE PCS      WITH PACK_HDR.TOT_PCS ;
            nCartons WITH PACK_HDR.TOT_CART

  ENDSCAN

  &&store

   && break relation after collecting Data
  SELECT PACK_LIN
  SET RELATION  TO
  SELECT Pack_Hdr
  SET RELATION TO
  SET ORDER TO &lcPckHdrOrd

ENDIF

WAIT CLEAR
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpInsRecs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Store WareHouse Code , City , State , Zip in memory variables
*!             also add records in Temp file
*!**************************************************************************
*! Example   : DO lpInsRecs
*!**************************************************************************
*
PROCEDURE lpInsRecs
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_SELECTING_RECORDS_SPACE_BAR_TO_ABORT NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECTING_RECORDS_SPACE_BAR_TO_ABORT,oAriaApplication.GetHeaderText("LANG_SELECTING_RECORDS_SPACE_BAR_TO_ABORT",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]


IF SEEK (lcWareCode,'WareHous')
  m.cWareDesc = WareHous.cDesc
  m.cWareAddr = IIF(EMPTY(WareHous.cAddress3),'',ALLTRIM(WareHous.cAddress3);
                +', '+ALLTRIM(WareHous.cAddress4))
  m.cWareZip  = ALLTRIM(WareHous.cAddress5)
ELSE
  STORE '' TO m.cWareDesc,m.cWareAddr,m.cWareZip
ENDIF

m.cCusVend  = IIF(SEEK('M'+lcRpAcct,'Customer'),Customer.cCusVend,'')
m.cDC_Name  = lcRpDistCt
IF SEEK('S'+lcRpAcct+m.cDC_Name,'Customer')
  m.cDC_Addr1 = Customer.cAddress1
  lcAddr3     = ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)+ALLTRIM(Customer.cAddress5)
  m.cDC_Addr2 = IIF(EMPTY(Customer.cAddress2),lcAddr3,ALLTRIM(Customer.cAddress2))
  m.cDC_Addr3 = IIF(EMPTY(Customer.cAddress2),'',lcAddr3)
ELSE
  m.cDC_Addr1 = ''
  m.cDC_Addr2 = ''
  m.cDC_Addr3 = ''
ENDIF

INSERT INTO (lcWorkFile) FROM MEMVAR
*-- End of lpInsRecs

*!***************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : = lfwRepWhen()
*!***************************************************************************
*
FUNCTION lfwRepWhen
lnInvPos   = lfItmPos('INVHDR.INVOICE')
lnDatInvPo = lfItmPos('INVHDR.INVDATE')
lnBillNoPo = lfItmPos('BOL_HDR.BOL_NO')

lnStorDCPo = lfVarPos('lcRpDistCt')

DO lpShowObj    && Enable/Disable all options in OG
DO lpCreatFil   && Create the Work Temp file

SELECT Pack_Hdr  && set the new index on the active master file
INDEX ON Account + Bill_Ladg TAG (lcPckIndex) OF (gcWorkDir+lcPckIndex+'.CDX')

SELECT InvHdr    && set the new index on the active master file
INDEX ON Account+cWareCode+ShipVia TAG (lcInvIndex) OF (gcWorkDir+lcInvIndex+'.CDX')
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Enable/Disable all options in OG
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
*
PROCEDURE lpShowObj
PRIVATE lcShowStat,llShwInRng,lnDisInv,lnDisInvDt,lnDisBOL

*-- Enable/Disable Distribution center in case of Bill of Lading [Begin.]
*-- If Position of Store DC greater than Zero (Exist)
IF lnStorDCPo > 0
  *-- If Print by Bill of Lading
  IF lcRpPrint = 'B'
    lcRpDistCt = ''
    laOGObjCnt[lnStorDCPo] = .F.
  ELSE     && Else Print by Invoice
    *-- If Account is Empty
    IF EMPTY(lcRpAcct)
      lcRpDistCt = ''  && to be sure there won't be valid store with Empty Account
      laOGObjCnt[lnStorDCPo] = .F.
    ELSE     && Else Account is not Empty
      laOGObjCnt[lnStorDCPo] = .T.
    ENDIF
  ENDIF
  =lfOGShowGet('lcRpDistCt')  && Show get Object .
ENDIF
*-- Enable/Disable Distribution center in case of Bill of Lading [End.]

*-- Enable/Disable Objects in case of Empty Account [Begin.]
*-- If Account is Empty
IF EMPTY(lcRpAcct)
  lcShowStat = "DISABLE"
  llShwInRng = .F.          && Flag to Enable/Disable Object
  *-- If Print by Bill of Lading
  IF lcRpPrint = 'B'
    *-- If BOL Range Position greater than zero (Exist)
    IF lnBillNoPo > 0
      lnDisBOL   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnBillNoPo)) + ",6]"),1)
      laOGObjCnt[lnDisBOL] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnBillNoPo)) + ',6]')
    ENDIF
  ELSE      && Else Print by Invoice
    *-- If Invoice Range Position greater than zero (Exist)
    IF lnInvPos > 0
      lnDisInv   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnInvPos)) + ",6]"),1)
      laOGObjCnt[lnDisInv] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnInvPos)) + ',6]')
    ENDIF

    *-- If Invoice Date Position greater than zero (Exist)
    IF lnInvPos > 0
      lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
      laOGObjCnt[lnDisInvDt] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')
    ENDIF
  ENDIF

ELSE      && Account is not empty
  llShwInRng = .T.
  *-- If Print by Bill of Lading
  IF lcRpPrint = 'B'
    lcShowStat = "ENABLE"
    *-- If BOL Range Position greater than zero (Exist)
    IF lnBillNoPo > 0
      lnDisBOL   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnBillNoPo)) + ",6]"),1)
      laOGObjCnt[lnDisBOL] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnBillNoPo)) + ',6]')
    ENDIF
  ELSE      && Else Print by Invoice
    *-- If Invoice Range Position greater than zero (Exist)
    IF lnInvPos > 0
      lnDisInv   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnInvPos)) + ",6]"),1)
      laOGObjCnt[lnDisInv] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnInvPos)) + ',6]')
    ENDIF

    *-- If Invoice Date Position greater than zero (Exist)
    IF lnInvPos > 0
      lnDisInvDt = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnDatInvPo)) + ",6]"),1)
      laOGObjCnt[lnDisInvDt] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDatInvPo)) + ',6]')
    ENDIF

    *-- if Store DC is Empty
    IF EMPTY(lcRpDistCt)
      lcShowStat = "DISABLE"
    ELSE
      lcShowStat = "ENABLE"
    ENDIF
  ENDIF
  *-- EndIf Print by Bill of Lading
ENDIF

SHOW GET pbOGPrevie &lcShowStat
SHOW GET pbRun      &lcShowStat
*-- Enable/Disable Objects in case of Empty Account [End.]
*-- End of lpShowObj.

*!***************************************************************************
*! Name      : lfPrintBy
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Validate for Print By
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example     : = lfPrintBy()
*!***************************************************************************
*
FUNCTION lfPrintBy
CLEARREAD()
*-- End of lfPrintBy.

*!***************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Create work cursor.
*!***************************************************************************
*! Called from : Report code.
*!***************************************************************************
*! Example   : DO lpCreatFil
*!***************************************************************************
*
PROCEDURE lpCreatFil
*cWareAddr  --> WareHouse City + WareHouse State
*cRoutePEPS --> Route to PEPS in Case of Print By (Bill of Lading) it will be
*               BOL_HDR.Carrier OR Invoice.Shipvia Description (Invoice case)
*cStoreNo   --> Store No in Case of Print By (Bill of Lading) it will be
*               Packing List Store OR Invoice Store (Invoice case)
*cStoreLoc  --> Store City + Store State
*cMerchDesc --> Merchandise Description (free format to be entered in OG)

*Define the cursor structure
DIMENSION laStruct[27,18]
lnI = 0
lnI = lnI + 1
laStruct[lnI,1] = 'BOL_NO'  &&17-digit BOL# : (the same logic used in the VICS standard BOL form "B")
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 17
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'DC'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 8
laStruct[lnI,4] = 0  && DC#: (on the BOL)

lnI = lnI + 1
laStruct[lnI,1] = 'CPUA'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 35
laStruct[lnI,4] = 0 &&  Load ID: (entered in the PUA field in the BOL > Shipment Info screen)

lnI = lnI + 1
laStruct[lnI,1] = 'cCustPo'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 15
laStruct[lnI,4] = 0  &&  Customer Order#: (CustPO)

lnI = lnI + 1
laStruct[lnI,1] = 'BtName'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 30
laStruct[lnI,4] = 0  &&  BtName

lnI = lnI + 1
laStruct[lnI,1] = 'SHIP_DATE'
laStruct[lnI,2] = 'D'
laStruct[lnI,3] = 8
laStruct[lnI,4] = 0  &&  Shipping date

lnI = lnI + 1
laStruct[lnI,1] = 'ORDER'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 6
laStruct[lnI,4] = 0  &&  Order #:

lnI = lnI + 1
laStruct[lnI,1] = 'DEPT'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 5
laStruct[lnI,4] = 0 &&  Customer Dept.

lnI = lnI + 1
laStruct[lnI,1] = 'cStoreNo'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 8
laStruct[lnI,4] = 0 &&  Ship To Store# (on each P/L)

lnI = lnI + 1
laStruct[lnI,1] = 'STYLE'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 19
laStruct[lnI,4] = 0  &&  Style #: (on each P/L)

lnI = lnI + 1
laStruct[lnI,1] = 'PACK_NO'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 6
laStruct[lnI,4] = 0  &&  Pick Ticket #: (which is the same as the P/L#)

lnI = lnI + 1
laStruct[lnI,1] = 'CARRIERCOD'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 4
laStruct[lnI,4] = 0  &&  SCAC: (Carrier code)

lnI = lnI + 1
laStruct[lnI,1] = 'CARRIER'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 40
laStruct[lnI,4] = 0  &&  Ship Via: get the description

lnI = lnI + 1
laStruct[lnI,1] = 'PCS'
laStruct[lnI,2] = 'N'
laStruct[lnI,3] = 8
laStruct[lnI,4] = 0  &&  Total Pcs (per each Packing List/Store)

lnI = lnI + 1
laStruct[lnI,1] = 'nCartons'
laStruct[lnI,2] = 'N'
laStruct[lnI,3] = 5
laStruct[lnI,4] = 0       &&  Total Cartons (per each Packing List/Store). , &&  Total Pieces and Cartons on the entire BOL.

lnI = lnI + 1
laStruct[lnI,1] = 'cWareDesc'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 35
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cWareAddr'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 18
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cWareZip'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 10
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cCusVend'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 15
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cRoutePEPS'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 40
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cStoreLoc'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 18
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'nWeight'
laStruct[lnI,2] = 'N'
laStruct[lnI,3] = 6
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cDC_Name'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 35
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cDC_Addr1'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 30
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cDc_Addr2'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 32
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cDc_Addr3'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 32
laStruct[lnI,4] = 0

lnI = lnI + 1
laStruct[lnI,1] = 'cGroupKey'
laStruct[lnI,2] = 'C'
laStruct[lnI,3] = 17
laStruct[lnI,4] = 0

FOR lnJ = 1 TO ALEN(laStruct,1)
  STORE .F. TO laStruct[lnJ,5],laStruct[lnJ,6]
  FOR lnK = 7 TO 16
    laStruct[lnJ,lnK] = ''
  ENDFOR
  STORE 0 TO laStruct[lnJ,17],laStruct[lnJ,18]
ENDFOR

DIMENSION laIndx[2,2]
laIndx[1,1] = 'cGroupKey + cStoreNo'
laIndx[1,2] = lcWorkFile
laIndx[2,1] = 'BOL_NO+PACK_NO+DC+CPUA+cCustPo+ORDER+DEPT+cStoreNo+STYLE'
laIndx[2,2] = 'BYBOL'
=gfCrtTmp(lcWorkFile,@laStruct,@laIndx)

*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Account Validation.
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : =lfvAccount()
*!***************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal
LOCAL o
o = _Screen.ActiveForm.ActiveControl
lcObjVal = o.Value
lcOldVal = o.OldValue

*IF !(lcObjVal == lcOldVal)
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER

*!*	  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field

  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    *&lcObjName = lcObjVal
    o.Value = lcObjVal
  ENDIF
  lcRpAcct = o.Value

  IF !(lcObjVal == lcOldVal)
    lcRpDistCt = ''
    llClearInv = .T.  && Clear previous Invoice Range
    llClearBno = .T.  && Clear previous Bill no Range
  ENDIF


  DO lpShowObj    && Enable/Disable all options in OG

  SELECT CUSTOMER
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)
ENDIF
*-- End of lfvAccount.


*!***************************************************************************
*! Name      : lfvStoreDC
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Validate Distribution Center Store
*!***************************************************************************
*! Example   : =lfvStoreDC()
*!***************************************************************************
*
FUNCTION lfvStoreDC
PRIVATE xStore,lcObjVal
LOCAL o
o = _Screen.ActiveForm.ActiveControl
lcObjVal = o.Value
lcOldVal = o.OldValue

*-- Notes : Store variable must be named as
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER

*!*	  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field

  *IF The user want to Browse or if Store He/She entered is not in the file
  IF '?' $ lcObjVal OR (!EMPTY(lcObjVal) AND !SEEK('S'+lcRpAcct+lcObjVal,'CUSTOMER'))
    xStore   = lcObjVal
    IF !CusBrowS(lcRpAcct,.T.)
      STORE SPACE(8) TO xStore
      lcObjVal = lcOldVal
    ENDIF
    lcObjVal = xStore
*!*	    &lcObjName = lcObjVal
    o.Value =  lcObjVal
  ENDIF

  *-- Clear previous Invoice Range
  *-- Clear previous Bill no Range
  STORE !(lcObjVal == lcOldVal) TO llClearInv , llClearBno

  DO lpShowObj    && Enable/Disable all options in OG

  SELECT Customer
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)

ENDIF
*-- End of lfvStoreDC.

*!***************************************************************************
*! Name      : lfSRInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : control browse Invoices for InvHdr File
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRInv()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRInv
PARAMETERS lcParm
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT InvHdr

DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO TAG InvHdrA
    GO TOP
  CASE lcParm = 'R'  && Reset code
    llClearInv = .F.
    SET ORDER TO TAG (lcInvIndex)      && restore old Index
    SELECT (lnAlias)
ENDCASE
*-- End of lfSRInv.

*!***************************************************************************
*! Name      : lfSRBillNo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
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

*!***************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Function that we call when Close the option grid.
*!***************************************************************************
*! Called from : [Option Grid] < Close > button.
*!***************************************************************************
*! Example     : = lfClearRep()
*!***************************************************************************
*
FUNCTION lfClearRep
SELECT InvHdr
CLOSE INDEXES

SELECT Pack_Hdr
CLOSE INDEXES

IF FILE(gcWorkDir+lcInvIndex+'.CDX')
  ERASE (gcWorkDir+lcInvIndex+'.CDX')
ENDIF

IF FILE(gcWorkDir+lcPckIndex+'.CDX')
  ERASE (gcWorkDir+lcPckIndex+'.CDX')
ENDIF

*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
*-- End of lfClearRep.

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : to get the position of the Variable in OG
*!***************************************************************************
*! Example   : = lfVarPos()
*!***************************************************************************
*
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.

*!***************************************************************************
*! Name      : lfTotPages
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : to get the total number of pages
*!***************************************************************************
*! Called from : Report
*!***************************************************************************
*! Example     : = lfTotPages()
*!***************************************************************************
*: Notes : if a change is done in Frx Remember to
*:         1) Print the Report and count the number of lines per page
*:         2) Adjust the number of lines (Hardcoded) in lfTotPages()

*-- we add one page in case of 31 lines or its multiple 'cause we have to
*-- print an excess page for Group Footer
FUNCTION lfTotPages
PRIVATE lnRecNo,lnNoOfLns
STORE 0 TO lnRecNo,lnNoOfLns
SELECT (lcWorkFile)
lnRecNo = RECNO()
IF !(cGroupKey==lcGroupKey)
  lcGroupKey = cGroupKey
  SET ORDER TO TAG (lcWorkFile) DESCENDING
  =SEEK(lcGroupKey,lcWorkFile)

  SCAN REST WHILE cGroupKey + cStoreNo = lcGroupKey
    lnNoOfLns = lnNoOfLns+1
  ENDSCAN
  IF MOD(lnNoOfLns,30) = 0
    lnPages = lnNoOfLns/30 + 1
  ELSE
    IF lnNoOfLns = 29
      lnPages = CEILING(lnNoOfLns/30) + 1
    ELSE
      lnPages = CEILING(lnNoOfLns/30)
    ENDIF
  ENDIF
ENDIF
*B126363,1 NNA (End)

SET ORDER TO TAG (lcWorkFile) ASCENDING
GO lnRecNo
RETURN ''
*-- End of lfTotPages.

****************************************************************************************************************
*
*   FUNCTION lfGetCheckDigit
*
****************************************************************************************************************
FUNCTION lfGetCheckDigit
LPARAMETER lcUccNo, lcType
lcType = IIF(TYPE('lcType')='C',lcType,'O')

LOCAL      lnChkDigit ,lnSumOdd  ,lnSumEven ,lnCount
STORE 0 TO lnChkDigit ,lnSumOdd  ,lnSumEven ,lnTop

IF TYPE('lcUccNo') = 'C'
  lnTop = LEN(lcUccNo)
ENDIF

FOR lnCount = 1 TO lnTop STEP 2
  lnSumOdd  = lnSumOdd  + VAL(SUBSTR(lcUccNo,lnCount     , 1))
  lnSumEven = lnSumEven + VAL(SUBSTR(lcUccNo,lnCount + 1 , 1))
ENDFOR

IF lcType = 'O'
 lnChkDigit = MOD(lnSumOdd*3 + lnSumEven , 10)
ELSE
 lnChkDigit = MOD(lnSumOdd + lnSumEven*3 , 10)
ENDIF
RETURN(IIF(lnChkDigit=0,'0',STR(INT(10-lnChkDigit),1)))


************************************************************************************************************************
*
*      FUNCTION lfvReportForm
*
************************************************************************************************************************
FUNCTION lfvReportForm
DO CASE
CASE lcRpName = 'ARJCPMB'
  lcLastPrintBy = lcRpPrint
  lcRpPrint = 'B'
  SET ORDER TO BYBOL IN (lcWorkFile)
CASE lcRpName = 'ARJCPMN'
  lcRpPrint = IIF(!EMPTY(lcLastPrintBy),lcLastPrintBy,lcRpPrint)
  SET ORDER TO (lcWorkFile) IN (lcWorkFile)
ENDCASE
CLEARREAD()
