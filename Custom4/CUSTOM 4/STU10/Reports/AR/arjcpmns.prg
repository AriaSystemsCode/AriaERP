*:***************************************************************************
*: Program file  : ARJCPMNS
*: Program desc. : Shipping Manifest Report For JCPENNEY (STU10)
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Albert Raif (ALB)
*: Date          : 06/09/2003
*: Reference     : C102828
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
*:***************************************************************************
*
*-- If user changed filter from OG [Begin.]

IF llOGFltCh
  PRIVATE lcMasterFl
  lnRowNo  = 35
  lnPages = 0
  lnNoOfLns = 0
  lcGroupKey = ' '
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF


lcSeekB=" Account + Pack_no = lcRpAcct "
lcSeekI=" Account + Invoice = lcRpAcct  AND InvHdr.Status <> 'V' "

lcInv= lfCheckFilter(1, 'INVHDR.INVOICE')  	
llInv   = !EMPTY(lcInv) AND USED(lcInv) AND RECCOUNT(lcInv) > 0
IF llInv   
  SELECT (lcInv)
  INDEX ON invoice TAG (lcInv)
  lcSeekI=lcSeekI+" AND SEEK(invoice,'"+lcInv+"')"
ELSE
  IF TYPE("lcInv") = "C" AND USED(lcInv)
    USE IN (lcInv)
  ENDIF
  lcInv= ''
ENDIF


lnDatePos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.INVDATE'),1)
LDATE = SUBSTR(laOGFxFlt[lnDatePos,6],1,ATC('|',laOGFxFlt[lnDatePos,6])-1)
HDATE = SUBSTR(laOGFxFlt[lnDatePos,6],  ATC('|',laOGFxFlt[lnDatePos,6])+1)

IF !EMPTY(LDATE)
  lcSeekI=lcSeekI +" AND  BETWEEN(INVDATE,CTOD('"+LDATE +"'),CTOD('"+HDATE+"')) "
ELSE
  IF  !EMPTY(HDATE)
    lcSeekI=lcSeekI +" AND  DATE<=CTOD('"+INVDATE+"') "
  ENDIF
ENDIF 

lcBOL= lfCheckFilter(1, 'BOL_HDR.BOL_NO')  	
llBOL   = !EMPTY(lcBOL) AND USED(lcBOL) AND RECCOUNT(lcBOL) > 0
IF llBOL   
  SELECT (lcBOL)
  INDEX ON BOL_NO TAG (lcBOL)
  lcSeekB=lcSeekB+" AND SEEK(BOL_HDR.BOL_NO ,'"+lcBOL+"')"
ELSE
  IF TYPE("lcBOL") = "C" AND USED(lcBOL)
    USE IN (lcBOL)
  ENDIF
  lcBOL= ''
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
   loogScroll.cCROrientation = 'P'
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
            m.cGroupKey,m.cPikTkt
STORE 0 TO m.nCartons,m.nWeight,m.nTotQty

*-- Collect data according to Type of Print By [Begin.]
*-- If Print by Bill of Lading
IF lcPrintTyp = 'B'
*!*	  lcRpExp = [Account + Pack_no = lcRpAcct AND ] + lcRpExp + ;
*!*	            [ AND !EOF('BOL_Hdr')]
  lcSeekB=lcSeekB+[ AND !EOF('BOL_Hdr')]
  PRIVATE lcBillNo
  GO TOP
  *-- Scan loop on Master file (Pack_Hdr) [Begin.]
  SCAN FOR &lcSeekB WHILE INKEY() <> 32
    lcWareCode   = BOL_Hdr.W_Code
    m.cRoutePEPS = IIF(EMPTY(BOL_Hdr.Carrier) AND !EMPTY(BOL_Hdr.ShipVia),gfCodDes(BOL_Hdr.ShipVia,'SHIPVIA'),BOL_Hdr.Carrier)
    
    m.cGroupKey  = lcRpAcct + Pack_Hdr.Bill_Ladg
    
    *-- If (Bill of lading) Store Distr. Center will be BOL_Hdr Store
    lcRpDistCt = BOL_Hdr.Store

    m.nCartons  = Tot_Cart
    m.nWeight   = Tot_Wght
    m.nTotQty    = Tot_Pcs
    m.cPikTkt   = PikTkt
    m.cCustPo   = IIF(SEEK('O'+Pack_hdr.Order,'OrdHdr'),OrdHdr.CustPo,'')    
    m.cStoreNo  = Store
    m.cStoreLoc = IIF(SEEK('S'+lcRpAcct+m.cStoreNo,'Customer'),;
                  IIF(EMPTY(Customer.cAddress3),ALLTRIM(Customer.cAddress4),;
                  ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)),'')

    DO lpInsRecs      && Insert record into the temporary file
  ENDSCAN    
  *-- Scan loop on Master file (Pack_Hdr) [End.]

ELSE    *-- Else Print by Invoice

*!*	  lcRpExp = [Account + Invoice = lcRpAcct AND ;
*!*	            InvHdr.Status <> "V" AND ] + lcRpExp
  PRIVATE lcShipVia
  GO TOP
  *-- Scan loop on Master file (InvHdr) [Begin.]

  SCAN FOR &lcSeekI WHILE INKEY() <> 32
    lcShipVia    = ShipVia
    lcWareCode   = cWareCode

    m.cRoutePEPS = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
    m.cGroupKey  = lcRpAcct+lcWareCode+lcShipVia

    m.nCartons  = Cartons
    m.nWeight   = Weight
    m.nTotQty    = Ship
    m.cPikTkt   = PikTkt
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
IF lcGroupKey <> cGroupKey
  lnNoOfLns = lnNoOfLns + 11
ELSE
  lnNoOfLns = lnNoOfLns + 1
ENDIF

IF lnNoOfLns >= lnRowNo
  lnPages = lnPages + 1
  lnNoOfLns = 0
ENDIF
lcGroupKey = cGroupKey

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
INDEX ON Account + Bill_Ladg TAG (lcPckIndex) OF (oAriaApplication.WorkDir+lcPckIndex+'.CDX')

SELECT InvHdr    && set the new index on the active master file    
INDEX ON Account+cWareCode+ShipVia TAG (lcInvIndex) OF (oAriaApplication.WorkDir+lcInvIndex+'.CDX')
lnPages = 0
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
  lcShowStat = ".F."
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
    lcShowStat = ".T."
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
      lcShowStat = ".F."
    ELSE
      lcShowStat = ".T."
    ENDIF
  ENDIF  
  *-- EndIf Print by Bill of Lading
ENDIF

loogscroll.parent.ogtoolbar.cntPrint.cmdprint.enabled =&lcShowStat
loogscroll.parent.ogtoolbar.cntPrint.cmdpreview.enabled =&lcShowStat

*-- Enable/Disable Objects in case of Empty Account [End.]
*-- End of lpShowObj.



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

lcObjVal = EVALUATE(OGSYS18())  && Varible to hold  the value of the current GET field

IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = OGSYS18()           && Varible to hold  the name of the memory variable used to create the current GET field
  
  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName  = lcObjVal
    lcRpAcct= lcObjVal

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
*!*	_screen.Visible = .T.
*!*	ACTIVATE WINDOW trace
*!*	susp

lcObjVal = EVALUATE(OGSYS18())  && Varible to hold  the value of the current GET field

*-- Notes : Store variable must be named as 
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = OGSYS18()           && Varible to hold  the name of the memory variable used to create the current GET field
  
  *IF The user want to Browse or if Store He/She entered is not in the file
  IF '?' $ lcObjVal OR (!EMPTY(lcObjVal) AND !SEEK('S'+lcRpAcct+lcObjVal,'CUSTOMER'))
    xStore   = lcObjVal
    IF !CusBrowS(lcRpAcct,.T.)
      STORE SPACE(8) TO xStore
      lcObjVal = lcOldVal
    ENDIF
    lcObjVal = xStore
    &lcObjName = lcObjVal
    lcRpDistCt= lcObjVal
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
   cDc_Addr2 C(32),cDc_Addr3 C(32), cGroupKey C(17) ,cPikTkt C(6),nTotQty N(8,0))
SELECT (lcWorkFile)
ZAP

INDEX ON cGroupKey + cStoreNo TAG (lcWorkFile)

*-- End of lpCreatFil.

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
CLEARREAD()

*-- End of lfPrintBy.

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

lcOldVal =EVALUATE(OGSYS18())
*-- End of lfwOldVal.
*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************

FUNCTION lfCheckFilter()
  LPARAMETERS lnArrayType, lcFilter
  LOCAL lcReturn, lnPOS 	
  DO CASE
	CASE lnArrayType = 1 
	  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
        lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 2  
	  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
        lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	CASE lnArrayType = 3  
	  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  	  IF lnPos > 0
        lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
        lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
	  ELSE
  	    lcReturn = ""	   
      ENDIF
	OTHERWISE :
		lcReturn = ""
  ENDCASE	
  RETURN lcReturn

