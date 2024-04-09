*:***************************************************************************
*: Program file  : ARJCPMN
*: Program desc. : Shipping Manifest Report For JCPENNEY
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 12/05/1999
*: Reference     : C101691
*:***************************************************************************
*: Calls : 
*:    Procedures : CusBrowM, CusBrowS, lpCollData, lpInsRecs, lpShowObj,
*:                 lpCreatFil
*:                   
*:    Functions  : gfModalGen, lfwRepWhen, lfvAccount, lfPrintBy, lfwOldVal,
*:               : lfvStoreDC, lfSRInv, lfSRBillNo, lfClearRep.
*:***************************************************************************
*: Example : DO ARJCPMN
*:***************************************************************************
*: Notes : if a change is done in Frx Remember to 
*:         1) Print the Report and count the number of lines per page 
*:         2) Adjust the number of lines (Hardcoded) in lfTotPages()
*:***************************************************************************
*: Modifications      :
*B603406,1 WAM 01/27/2000  Get BOL Carrier from the new filed ShipVia in the BOLHDR file
*B803062,1 AHM 02/18/2000  Make the sort order on the store
*B804065,1 BWA 04/01/2001  Change the size page to be LETTER 8.5*11 Not A4 LETTER [FIX IN FRX]
*B804528,1 ADEL 11/13/01   Fix the Shipto address. (In PRG and FRX : Add Address3)
*B126363,1 NNA 02/03/2005  Fix Bug of Wrong total pages in (Page X of XX)
*:***************************************************************************
*
*-- If user changed filter from OG [Begin.]

IF llOGFltCh
  PRIVATE lcMasterFl
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF

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

ENDIF
*-- EndIf of user changed filter from OG [End.]
*-- If no records in temp file (empty)
SELECT (lcWorkFile)
*-- If Seek is successful (There's Records)
IF SEEK(lcRpAcct)
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
IF TYPE('lcPrintTyp') $ 'UL'
  RETURN
ENDIF

PRIVATE lcWareCode
*-- Initializing memory variables used in the Temp File
STORE '' TO m.cWareDesc,m.cWareAddr,m.cWareZip,m.cCusVend,;
            m.cRoutePEPS,m.cStoreNo,m.cStoreLoc,m.cCustPo,;
            m.cDC_Name,m.cDC_Addr1,m.cDC_Addr2,cDC_Addr3,m.cGroupKey
STORE 0 TO m.nCartons,m.nWeight

*-- Collect data according to Type of Print By [Begin.]
*-- If Print by Bill of Lading
IF lcPrintTyp = 'B'
  lcRpExp = [Account + Pack_no = lcRpAcct AND ] + lcRpExp + ;
            [ AND !EOF('BOL_Hdr')]
  
  PRIVATE lcBillNo
  GO TOP
  *-- Scan loop on Master file (Pack_Hdr) [Begin.]
  SCAN FOR &lcRpExp WHILE INKEY() <> 32
    lcWareCode   = BOL_Hdr.W_Code
    *B603406,1 Get BOL Carrier from the new filed ShipVia in the BOLHDR file
    *m.cRoutePEPS = BOL_Hdr.Carrier
    m.cRoutePEPS = IIF(EMPTY(BOL_Hdr.Carrier) AND !EMPTY(BOL_Hdr.ShipVia),gfCodDes(BOL_Hdr.ShipVia,'SHIPVIA'),BOL_Hdr.Carrier)
    *B603406,1 (End)
    
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
  *-- Scan loop on Master file (InvHdr) [Begin.]
ENDIF
*-- Collect data according to Type of Print By [End.]

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
WAIT WINDOW 'Selecting records ...<Space Bar> to abort' NOWAIT    

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
  *B804528,1 (Begin) Print DC name, Address1, Address2, Address3.
  *m.cDC_Addr2 = IIF(EMPTY(Customer.cAddress3),'',ALLTRIM(Customer.cAddress3)+', ');
                +IIF(EMPTY(Customer.cAddress4),'',ALLTRIM(Customer.cAddress4)+', ');
                +ALLTRIM(Customer.cAddress5)
  lcAddr3     = ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)+ALLTRIM(Customer.cAddress5)
  m.cDC_Addr2 = IIF(EMPTY(Customer.cAddress2),lcAddr3,ALLTRIM(Customer.cAddress2))
  m.cDC_Addr3 = IIF(EMPTY(Customer.cAddress2),'',lcAddr3)
  *B804528,1 (End)
ELSE
  m.cDC_Addr1 = ''
  m.cDC_Addr2 = ''
  *B804528,1 (Begin) Initialize address3.
  m.cDC_Addr3 = ''
  *B804528,1 (End)
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
CLEAR READ
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

*B804528,1 (Begin) Add a field for Address3 (cDc_Addr3)
*CREATE CURSOR (lcWorkFile) ;
  (cWareDesc C(35), cWareAddr C(18), cWareZip C(10), cCusVend C(15), ;
   cRoutePEPS C(40), cStoreNo C(8), cStoreLoc C(18), nCartons N(5,0),;
   nWeight N(6,0), cCustPo C(15), cDC_Name C(35), cDC_Addr1 C(30),;
   cDc_Addr2 C(32), cGroupKey C(17))
CREATE CURSOR (lcWorkFile) ;
  (cWareDesc C(35), cWareAddr C(18), cWareZip C(10), cCusVend C(15), ;
   cRoutePEPS C(40), cStoreNo C(8), cStoreLoc C(18), nCartons N(5,0),;
   nWeight N(6,0), cCustPo C(15), cDC_Name C(35), cDC_Addr1 C(30),;
   cDc_Addr2 C(32),cDc_Addr3 C(32), cGroupKey C(17))
*B804528,1 (End)
SELECT (lcWorkFile)
ZAP

*B803062,1 (Start) 02/18/00
*INDEX ON cGroupKey TAG (lcWorkFile)
INDEX ON cGroupKey + cStoreNo TAG (lcWorkFile)
*B803062,1 (End)

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
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  
  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF 
  
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
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/05/1999
*! Purpose   : Capture current object old value.
*!***************************************************************************
*! Called from : validation of Account OR Store only
*!***************************************************************************
*! Example   : =lfwOldVal()
*!***************************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal.

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
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*-- Notes : Store variable must be named as 
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  
  *IF The user want to Browse or if Store He/She entered is not in the file
  IF '?' $ lcObjVal OR (!EMPTY(lcObjVal) AND !SEEK('S'+lcRpAcct+lcObjVal,'CUSTOMER'))
    xStore   = lcObjVal
    IF !CusBrowS(lcRpAcct,.T.)
      STORE SPACE(8) TO xStore
      lcObjVal = lcOldVal
    ENDIF
    lcObjVal = xStore
    &lcObjName = lcObjVal
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

  *B126363,1 NNA 02/03/2005 (BEGIN) scan to get the total lines for the current manifest then
  *B126363,1 NNA            get the Total Pages No.  
  *lnNoOfLns = RECNO() - lnRecNo + 1
  *IF MOD(lnNoOfLns,31) = 0
  *  lnPages = lnNoOfLns/31 + 1
  *ELSE
  *  IF lnNoOfLns = 30
  *    lnPages = CEILING(lnNoOfLns/31) + 1
  *  ELSE
  *    lnPages = CEILING(lnNoOfLns/31)
  *  ENDIF  
  *ENDIF
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
