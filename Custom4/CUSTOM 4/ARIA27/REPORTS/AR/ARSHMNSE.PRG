*:***************************************************************************
*: Program file  : ARSHMNSE
*: Program desc. : Shipping Manifest Report For Sears
*: System        : Aria Advantage Series.
*: Module        : Account Receivable (AR)
*: Developer     : Mostafa Mahmoud Rawash (MMR)
*: Date          : 12/30/2005
*: Reference     : E040072
*:***************************************************************************
*: Calls : 
*:    Procedures : CusBrowM, CusBrowS, lpCollData, lpInsRecs, lpShowObj,
*:                 lpCreatFil
*:                   
*:    Functions  : gfModalGen, lfwRepWhen, lfvAccount, lfPrintBy, lfwOldVal,
*:               : lfvStoreDC, lfSRBillNo, lfClearRep.
*:***************************************************************************
*: Example : DO ARSHMNSE
*:***************************************************************************
*: Notes : if a change is done in Frx Remember to 
*:         1) Print the Report and count the number of lines per page 
*:         2) Adjust the number of lines (Hardcoded) in lfTotPages()
*:***************************************************************************
*:Modifications ..
*:***************************************************************************
*-- If user changed filter from OG [Begin.]
IF llOGFltCh
  PRIVATE lcMasterFl
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF

  *-- Case Print by BOL
    lcMasterFl = 'Pack_Hdr'
    SELECT Pack_Hdr
    SET RELATION TO Account + Bill_ladg INTO BOL_Hdr
    DO lpCollData WITH 'B'

    SELECT Pack_Hdr
    SET RELATION TO     && break relation after collecting Data

ENDIF
*-- EndIf of user changed filter from OG [End.]
  lcRpName = 'ARSHMNSE'
  = gfCrtFrm(lcRpName,lcOGFormArr,llOGRefForm)  
  = lfRepPltFr(lcRpName)

*-- If no records in temp file (empty)
SELECT (lcWorkFile)
*-- If Seek is successful (There's Records)
IF SEEK(lcRpAcct)
  GO TOP
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
            m.cDC_Name,m.cDC_Addr1,m.cDC_Addr2,cDC_Addr3,m.cGroupKey,m.cBol,m.cCity,m.cState,cOldAlis
STORE 0 TO m.nCartons,m.nWeight

*-- Collect data according to Type of Print By [Begin.]
*-- If Print by Bill of Lading
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
    m.nWeight   = Tot_Wght
    m.cCustPo   = IIF(SEEK('O'+Pack_hdr.Order,'OrdHdr'),OrdHdr.CustPo,'')    
    m.cStoreNo  = Store
    m.cStoreLoc = IIF(SEEK('S'+lcRpAcct+m.cStoreNo,'Customer'),;
                  IIF(EMPTY(Customer.cAddress3),ALLTRIM(Customer.cAddress4),;
                  ALLTRIM(Customer.cAddress3)+', '+ALLTRIM(Customer.cAddress4)),'')
    m.cBol      = BOL_HDR.bol_no 
    DO lpInsRecs      && Insert record into the temporary file
  ENDSCAN    
  *-- Scan loop on Master file (Pack_Hdr) [End.]

*-- Collect data according to Type of Print By [End.]

WAIT CLEAR
*-- End of lpCollData.

*!**************************************************************************
*! Name      : lpInsRecs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
*! Purpose   : Store WareHouse Code , City , State , Zip in memory variables
*!             also add records in Temp file 
*!**************************************************************************
*! Example   : DO lpInsRecs
*!**************************************************************************
*
PROCEDURE lpInsRecs
WAIT WINDOW 'Selecting records ...<Space Bar> to abort' NOWAIT    

PRIVATE lnAlias
lnAlias = SELECT()
IF SEEK (lcWareCode,'WareHous')
  m.cWareDesc = WareHous.cDesc
  
  m.cWareAddr = IIF(EMPTY(WareHous.cAddress3),'',ALLTRIM(WareHous.cAddress3);
                +', '+ALLTRIM(WareHous.cAddress4))
                
    m.cWareAddr =  IIF(EMPTY(WareHous.cAddress1),'',ALLTRIM(WareHous.cAddress1))
    m.cCity     =  IIF(EMPTY(WareHous.cAddress3),'',ALLTRIM(WareHous.cAddress3))
    m.cState    =  IIF(EMPTY(WareHous.cAddress4),'',ALLTRIM(WareHous.cAddress4))         
  ENDIF                
  m.cWareZip  = m.cCity +', '+m.cState +', ' +ALLTRIM(WareHous.cAddress5)

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
*-- In case of SEARS form.
  m.ACCOUNT  = Pack_hdr.Account
  m.cWareCode= BOL_Hdr.W_Code
  m.cShipVia = BOL_Hdr.ShipVia
  SELECT (lcWorkFile )
  LOCATE FOR Account+cStoreNo+cWareCode+cShipVia = m.ACCOUNT+m.cStoreNo+m.cWareCode+m.cShipVia
  IF FOUND()
    REPLACE &lcWorkFile..nWeight   WITH &lcWorkFile..nWeight + m.nWeight,;
            &lcWorkFile..nCartons  WITH &lcWorkFile..nCartons+ m.nCartons
    STORE 0 TO m.nWeight,m.nCartons
  ENDIF
INSERT INTO (lcWorkFile) FROM MEMVAR
    m.Dept =  IIF(SEEK('O'+Pack_hdr.Order,'OrdHdr'),OrdHdr.Dept,'')
    REPLACE &lcWorkFile..Dept WITH m.Dept
SELECT (lnAlias)
*-- End of lpInsRecs

*!***************************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : = lfwRepWhen()
*!***************************************************************************
*
FUNCTION lfwRepWhen
lnBillNoPo = lfItmPos('BOL_HDR.BOL_NO')
lnStorDCPo = lfVarPos('lcRpDistCt')
DO lpShowObj    && Enable/Disable all options in OG 
DO lpCreatFil   && Create the Work Temp file

SELECT Pack_Hdr  && set the new index on the active master file 
INDEX ON Account + Bill_Ladg TAG (lcPckIndex) OF (gcWorkDir+lcPckIndex+'.CDX')

*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
*! Purpose   : Enable/Disable all options in OG 
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
*
PROCEDURE lpShowObj
PRIVATE lcShowStat,llShwInRng,lnDisBOL

*-- Enable/Disable Distribution center in case of Bill of Lading [Begin.]
*-- If Position of Store DC greater than Zero (Exist)
IF lnStorDCPo > 0
  *-- If Print by Bill of Lading
    lcRpDistCt = ''
    laOGObjCnt[lnStorDCPo] = .F.
  =lfOGShowGet('lcRpDistCt')  && Show get Object .
ENDIF
*-- Enable/Disable Distribution center in case of Bill of Lading [End.]

*-- Enable/Disable Objects in case of Empty Account [Begin.]
*-- If Account is Empty
IF EMPTY(lcRpAcct)
  lcShowStat = "DISABLE"
  llShwInRng = .F.          && Flag to Enable/Disable Object
  *-- If Print by Bill of Lading
    *-- If BOL Range Position greater than zero (Exist)
    IF lnBillNoPo > 0
      lnDisBOL   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnBillNoPo)) + ",6]"),1)
      laOGObjCnt[lnDisBOL] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnBillNoPo)) + ',6]')    
    ENDIF               
  
ELSE      && Account is not empty
  llShwInRng = .T.
  *-- If Print by Bill of Lading
    lcShowStat = "ENABLE"
    *-- If BOL Range Position greater than zero (Exist)
    IF lnBillNoPo > 0
      lnDisBOL   = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,"laOGFxFlt[" + ;
                   ALLTRIM(STR(lnBillNoPo)) + ",6]"),1)
      laOGObjCnt[lnDisBOL] = llShwInRng
      = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnBillNoPo)) + ',6]')    
    ENDIF               
  *-- EndIf Print by Bill of Lading
ENDIF

SHOW GET pbOGPrevie &lcShowStat
SHOW GET pbRun      &lcShowStat
*-- Enable/Disable Objects in case of Empty Account [End.]
*-- End of lpShowObj.

*!***************************************************************************
*! Name      : lfPrintBy
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*               BOL_HDR.Carrier 
*cStoreNo   --> Store No in Case of Print By (Bill of Lading) it will be
*               Packing List Store 
*cStoreLoc  --> Store City + Store State
*cMerchDesc --> Merchandise Description (free format to be entered in OG)

CREATE CURSOR (lcWorkFile) ;
  (ACCOUNT C(5),cWareCode C(6),cShipVia C(6), cWareDesc C(35), cWareAddr C(50), cWareZip C(60), cCusVend C(15), ;
   cRoutePEPS C(40), cStoreNo C(8), cStoreLoc C(18), nCartons N(5,0),;
   nWeight N(6,0), Dept C(3),cCustPo C(15), cDC_Name C(35), cDC_Addr1 C(30),;
   cDc_Addr2 C(32),cDc_Addr3 C(32), cGroupKey C(17),cCity C(40) , cStore C(40) ,cBol C(6))

SELECT (lcWorkFile)
ZAP
INDEX ON cGroupKey + cStoreNo  TAG (lcWorkFile)

*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
  
  *-- Clear previous Bill no Range
  STORE !(lcObjVal == lcOldVal) TO llClearBno
  
  DO lpShowObj    && Enable/Disable all options in OG 
  
  SELECT Customer  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)

ENDIF  
*-- End of lfvStoreDC.


*!***************************************************************************
*! Name      : lfSRBillNo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
*! Purpose   : Function that we call when Close the option grid.
*!***************************************************************************
*! Called from : [Option Grid] < Close > button.
*!***************************************************************************
*! Example     : = lfClearRep()
*!***************************************************************************
*
FUNCTION lfClearRep

SELECT Pack_Hdr
CLOSE INDEXES


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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/29/02
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

IF !(cGroupKey==lcGroupKey)
  lcGroupKey = cGroupKey    

  SET ORDER TO TAG (lcWorkFile) DESCENDING
  =SEEK(lcGroupKey,lcWorkFile)
  lnNoOfLns = RECNO() - lnRecNo + 1
  
  lnNoOfLns = ABS(lnNoOfLns)
  
  IF MOD(lnNoOfLns,31) = 0
    lnPages = lnNoOfLns/31 + 1
  ELSE
    IF lnNoOfLns = 30
      lnPages = CEILING(lnNoOfLns/31) + 1
    ELSE
      lnPages = CEILING(lnNoOfLns/31)
    ENDIF  
  ENDIF
  SET ORDER TO TAG (lcWorkFile) ASCENDING
ENDIF

GO lnRecNo
RETURN ''
*-- End of lfTotPages.

FUNCTION lfvClrRd

CLEAR READ
