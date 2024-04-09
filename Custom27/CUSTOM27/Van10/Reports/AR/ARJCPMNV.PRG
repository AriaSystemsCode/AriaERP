*:***************************************************************************
*: Program file  : ARJCPMNS
*: Program desc. : Shipping Manifest Report For JCPENNEY (VAN10)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Albert Raif (ALB)
*: Date          : 06/09/2003
*: Reference     : C102829
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
*C037329,1 MHM 03/24/2004 Fix bug of No. of page not correct
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
            m.cDC_Name,m.cDC_Addr1,m.cDC_Addr2,cDC_Addr3 ,;
            m.cGroupKey,m.BOL_NO
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
    m.cRoutePEPS = IIF(EMPTY(BOL_Hdr.Carrier) AND !EMPTY(BOL_Hdr.ShipVia),gfCodDes(BOL_Hdr.ShipVia,'SHIPVIA'),BOL_Hdr.Carrier)
    
    m.cGroupKey  = lcRpAcct + Pack_Hdr.Bill_Ladg
    
    *-- If (Bill of lading) Store Distr. Center will be BOL_Hdr Store
    lcRpDistCt = BOL_Hdr.Store

    m.nCartons  = Tot_Cart
    m.nWeight   = IIF(Tot_Wght<=0.5,1,Tot_Wght)
    m.BOL_NO    = BILL_LADG
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
    m.nWeight   = IIF(Weight<=0.5,1,Weight)
    m.BOL_NO    = BOL_NO
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*cPikTkt    --> Pick ticket #
*nTotQty    --> Total Quantaty

CREATE CURSOR (lcWorkFile) ;
  (cWareDesc C(35), cWareAddr C(30), cWareZip C(10), cCusVend C(15), ;
   cRoutePEPS C(40), cStoreNo C(8), cStoreLoc C(18), nCartons N(5,0),;
   nWeight N(6,0), cCustPo C(15), cDC_Name C(35), cDC_Addr1 C(30),;
   cDc_Addr2 C(32),cDc_Addr3 C(32), cGroupKey C(17), BOL_NO C(6))
SELECT (lcWorkFile)
ZAP

INDEX ON cGroupKey + cStoreNo TAG (lcWorkFile)

*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
*! Developer : Albert Raif (ALB)
*! Date      : 06/09/2003
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
SELECT (lcWorkFile)
lnRecNo = RECNO()
PRIVATE lnRowNo

lnRowNo  = 29

IF !(cGroupKey==lcGroupKey)
  lcGroupKey = cGroupKey    

  SET ORDER TO TAG (lcWorkFile) DESCENDING
  =SEEK(lcGroupKey,lcWorkFile)
  
  *C037329,1 MHM 03/24/2004 Fix bug of No. of page not correct[Start]
  *lnNoOfLns = RECNO() - lnRecNo + 1
  lnNoOfLns = 1
  SCAN REST WHILE cGroupKey = lcGroupKey
    lnNoOfLns=1+lnNoOfLns
  ENDSCAN
  *C037329,1 [End]
  
  IF MOD(lnNoOfLns,lnRowNo) = 0
    lnPages = lnNoOfLns/lnRowNo + 1
  ELSE
    IF lnNoOfLns = lnRowNo - 1
      lnPages = CEILING(lnNoOfLns/lnRowNo) + 1
    ELSE
      lnPages = CEILING(lnNoOfLns/lnRowNo)
    ENDIF  
  ENDIF
  SET ORDER TO TAG (lcWorkFile) ASCENDING
ENDIF

GO lnRecNo
RETURN ''
*-- End of lfTotPages.
