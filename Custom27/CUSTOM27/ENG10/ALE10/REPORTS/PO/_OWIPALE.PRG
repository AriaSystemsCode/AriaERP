****************************************************************************
*: Program file      : POWIPALE.PRG   (C#200156)
*: Program desc.     : WORK IN PROGRESS REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : MOHAMED SHOKRY
*: Date              : 02/05/2001
*:**************************************************************************
*:Modifications :
*:****************************************************************************************
*:C200168,1 MHM 02/20/2001 1)Adding the cut off option date.
*:C200168,1                2)Adding two new options Cut off date and print 
*:C200168,1                detail (yes/no). Changing the Frx to hold the details.
*:****************************************************************************************

*-- Create temp File
STORE '' TO lcPoNo, lcCustPo
lcTmpFile = gfTempName()

*:C200168,1  MHM 02/20/2001  add new fields to cursor in case of print detail Yes [START]
*CREATE CURSOR (lcTmpFile);
   ( dComplete D, cCustPo C(15), cBTName C(30) , cOrder C(6) ,dEntered D, cStyMaj C(12) , cDesc C(20),;
    cVendor C(30) , cPO C(6) , nTOTQty N(10) , nValue N(13,2) )
*INDEX ON DTOS(dComplete)+ cPO + cOrder + cStyMaj  TAG (lcTmpFile) OF (lcTmpFile)
IF llRpDetal
  CREATE CURSOR (lcTmpFile);
      ( dComplete D, cCustPo C(15), cBTName C(30) , cOrder C(6) ,dEntered D, cStyMaj C(12) , cDesc C(20),;
      cVendor C(30) , cPO C(6) , nTOTQty N(10) , nValue N(13,2) ,cItem C(19) ,cIclr C(6), cItmDesc C(20) ,;
      nUntCost N(11,3) , nReqQty N(12,3) , nUsedQty N(12,3) ,nEstCost N(12,2) ,;
      nActCost N(12,2), cType C(1),cCatgTyp C(1),lPrint L , lPrntTot L)
  *INDEX ON DTOS(dComplete)+ cPO + cOrder + cStyMaj+cType  TAG (lcTmpFile) OF (lcTmpFile)
  INDEX ON DTOS(dComplete)+ cPO + cOrder + cType + cStyMaj  TAG (lcTmpFile) OF (lcTmpFile)
ELSE      
  CREATE CURSOR (lcTmpFile);
     ( dComplete D, cCustPo C(15), cBTName C(30) , cOrder C(6) ,dEntered D, cStyMaj C(12) , cDesc C(20),;
      cVendor C(30) , cPO C(6) , nTOTQty N(10) , nValue N(13,2) , lPrntTot L)
  INDEX ON DTOS(dComplete)+ cPO + cOrder + cStyMaj  TAG (lcTmpFile) OF (lcTmpFile)
ENDIF
*:C200168,1  MHM 02/20/2001 [END]


*-- To get the selected vendors if any
lcVendFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcVendFile) AND USED(lcVendFile)
    SELECT (lcVendFile)
    GOTO TOP
  ENDIF  
ENDIF

*-- To get the selected customer if any.
lcCustFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcCustFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcCustFile) AND USED(lcCustFile)
    SELECT (lcCustFile)
    GOTO TOP
  ENDIF  
ENDIF

IF !EMPTY(lcVendFile) AND USED(lcVendFile) AND !EOF(lcVendFile)
  =lfDataByVn('V')
ELSE
  IF !EMPTY(lcCustFile) AND USED(lcCustFile) AND !EOF(lcCustFile)
    =lfDataByCs('C')
  ELSE
    =lfDataByAl('V')
  ENDIF
ENDIF

SELECT(lcTmpFile)
GOTO TOP
IF EOF()
  SET DEVICE TO SCREEN
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
GOTO BOTT
REPLACE lPrntTot WITH .T.

GOTO TOP
*--- Start printing report
DO gfDispRe WITH EVAL('lcRpName')
SET DEVICE TO SCREEN

****************************************************************************
*! PROG      : lfDataByCs.PRG   (C#200156)
*! DESC      : function To collect data in the temporary file
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByCs
PARAMETERS lcParam
PRIVATE lcOldOrd,lcCustOrd ,lcPoOrd

SELECT OrdHdr
lcCustOrd = SET('ORDER')
SET ORDER TO Ordacct

SELECT CutPick
lcOldOrd = SET('ORDER')
SET ORDER TO Cutord

SELECT PosHdr
lcPoOrd = SET('ORDER')
SET ORDER TO PosHdr


SELECT (lcCustFile)
GOTO TOP
SCAN
  lcCustomer = Account
  IF SEEK(lcCustomer,'OrdHdr')
    SELECT OrdHdr
    SCAN REST WHILE account+cordtype+order = lcCustomer
      IF SEEK('2'+OrdHdr.Order,'CutPick')
        lcPoNo = CutPick.cTktNo
        IF SEEK('P'+lcPoNo,'PosHdr') AND PosHdr.Status = 'O'
          lcPoNo = PosHdr.PO          
          =lfGetPoLin()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

SELECT OrdHdr
SET ORDER TO &lcCustOrd

SELECT CutPick
SET ORDER TO &lcOldOrd

SELECT PosHdr
SET ORDER TO &lcPoOrd
****************************************************************************
*! PROG      : lfDataByAl.PRG   (C#200156)
*! DESC      : function To collect all lines from posln
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfDataByAl
PARAMETERS lcParam
SELECT PosHdr
SCAN FOR Status =  "O"
  lcPoNo = PosHdr.PO
  =lfGetPoLin()
ENDSCAN

****************************************************************************
*! PROG      : lfDataByAl.PRG   (C#200156)
*! DESC      : function To scan for the selected vendors
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*-- 
FUNCTION lfDataByVn
PARAMETERS lcParam
SELECT (lcVendFile)
GOTO TOP
*-- If there are selected vendors
IF !EOF()    
  SCAN
    lcVendCode = cVendCode
    IF SEEK(lcVendCode,'POSHDR')        
      SELECT PosHdr        
      SCAN REST WHILE Vendor+cStyType+PO = lcVendCode FOR Status $ "O"
        lcPoNo = PosHdr.PO
        =lfGetPoLin()
      ENDSCAN        
    ENDIF      
  ENDSCAN
ENDIF  

****************************************************************************
*! PROG      : lfReltdDat.PRG   (C#200156)
*! DESC      : function To get the related data from the style,cutpick,apvendor,
*!           : ordhdr,ordline and insert a new record in the temporary file
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfReltdDat
PARAMETERS lcType
PRIVATE llReturn, lnAlias

lcCustPO = ''
lnAlias  = SELECT()
llReturn = .T.
=SEEK(Style,'Style')
=SEEK(PosHdr.Vendor,'ApVendor')
IF lcType = 'V'
  =SEEK('2'+PosHdr.PO+STR(PosLn.LineNo,6),'CutPick')
  =SEEK('O'+CutPick.Order,'OrdHdr')

  IF !EMPTY(lcCustFile) AND USED(lcCustFile)
    SELECT(lcCustFile)
    GOTO TOP
    IF !SEEK(OrdHdr.Account,lcCustFile)
      llReturn = .F.
      SELECT(lnAlias)
      RETURN llReturn
    ENDIF
  ENDIF
ENDIF

IF OrdHdr.Multi = 'Y'
  =SEEK('S'+OrdHdr.Account,'Customer')
ELSE
  =SEEK('M'+OrdHdr.Account,'Customer')
ENDIF

IF !(OrdHdr.MultiPO)
  lcCustPO = OrdHdr.CustPo
ELSE
  =SEEK('O'+CutPick.Order+PADL(CutPick.cTktLineNo,6,' '),'OrdLine')
  lcCustPO = OrdLine.CustPo
ENDIF

INSERT INTO (lcTmpFile);
      (dComplete,cCustPo,cBTName,cOrder,cStyMaj,cDesc,cVendor,cPO,dEntered);
VALUES ;
      (PosHdr.Complete,lcCustPO,Customer.BtName,OrdHdr.Order,;
       SUBSTR(PosLn.Style,1,12),Style.Desc,ApVendor.cVenComp,PosHdr.PO,OrdHdr.Entered)

SELECT(lnAlias)
RETURN llReturn

*!*************************************************************
*! Name      : lfsrvVen
*! Developer : MOHAMED SHOKRY
*! Date      : 02/08/2001
*! Purpose   : Rise change Vendor flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvVen()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVVen
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN APVENDOR
ENDIF
*-- End Of lfSRVVen.


****************************************************************************
*! PROG      : lfGetPoLin.PRG   (C#200156)
*! DESC      : function To collect lines from posln
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfReltdDat.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfGetPoLin
PRIVATE lnAlias

lnAlias = SELECT()
STORE 0 TO lnTotQty,lnValue
*:C200168,1 MHM 02/20/2001 variable to check if update record [start] 
llUpdatRec = .F.
*:C200168,1  MHM 02/20/2001 [end]
IF SEEK('P'+lcPoNo,'PosLn')
  
  lnValue = lfGetValue(lcPoNo)
  
  SELECT PosLn
  DO WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO 
  
    *-- To get the related data from the style,cutpick,apvendor,ordhdr,ordline
    *-- and insert a new record in the temporary file
    IF !lfReltdDat(lcParam)
      IF !EOF()
        SKIP
        LOOP
      ENDIF
    ENDIF
    *:C200168,1 MHM 02/20/2001[start]
    llUpdatRec = .T.
    *:C200168,1 MHM 02/20/2001 [end ]
    lnTotQty = 0
    lcStyKey = cstytype+PO+SUBSTR(Posln.Style,1,12)
    SCAN REST WHILE cstytype+po+Posln.style+STR(lineno,6)+trancd = lcStyKey
      SELECT (lcTmpFile)
      IF PosLn.TranCd='1'
        REPLACE nTotQty WITH nTotQty + PosLn.TotQty
        lnTotQty = lnTotQty + PosLn.TotQty
      ELSE
      
        *:C200168,1 MHM 02/20/2001 add cut off date to get total with [start]
        *REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)
        IF !EMPTY(ldRpCutOFF) 
          IF PosLn.Date <= ldRpCutOFF
            REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)
          ENDIF  
        ELSE
          REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)        
        ENDIF
        *:C200168,1 MHM 02/20/2001 [end]
      ENDIF                    
    ENDSCAN
  ENDDO
  
  *:C200168,1 MHM 02/20/2001 [start]
  lnTotVal = IIF(lnTotQty <> 0,lnValue / lnTotQty,0)
  SELECT (lcTmpFile)
  REPLACE nValue WITH lnTotVal * nTotQty
  *:C200168,1 MHM 02/20/2001 [End]
  
ENDIF

*:C200168,1 MHM 02/20/2001 [start]
IF llRpDetal AND llUpdatRec = .T.
  =lfCostItem()
ENDIF
*:C200168,1 MHM 02/20/2001 [end]

SELECT (lnAlias)

****************************************************************************
*! PROG      : lfGetValue.PRG   (C#200156)
*! DESC      : function To GET VALUE
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/08/2001
****************************************************************************
*! Called from : lfGetValue.PRG
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfGetValue
PARAMETERS lcPo
PRIVATE lnAlias, lnVal

lnAlias = SELECT()
lnVal   = 0
IF SEEK(lcPo,'MATINVJL')
  SELECT MatInvJl
  SUM REST (nUnitCost*nissued) - (nunitcost * nreceived) TO lnVal ;
  WHILE cTran+cOprCode+cLotNo+cTranType+cFabric+cColor+cWareCode = lcPo;
  FOR CTranType = "4"
ENDIF
SELECT(lnAlias)
RETURN lnVal


*!*************************************************************
*! Name      : lfCostItem             (C200168)
*! Developer : Mohamed Shokry
*! Date      : 02/20/2001
*! Purpose   : Calculate cost items.
*!*************************************************************
*! Called from : POWIPALE.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCostItem()
*!*************************************************************
FUNCTION lfCostItem
PRIVATE lnAlias, llPrint

llPrint = .T.
lnAlias = SELECT()
IF SEEK('I'+PosHdr.PO,'CTktBom')
  
  *-- To get the fabrics.
  SELECT cTktBom
  SCAN REST WHILE cImtyp+CutTkt+Typ+Item+Iclr+mfgcode+Dyelot = 'I'+PosHdr.PO;
            FOR cCatgTyp = 'F'
            
    INSERT INTO (lcTmpFile);
        (dComplete,cCustPo,cBTName,cOrder,cStyMaj,cDesc,cVendor,cPO,dEntered,;
         lPrint) ;
    VALUES ;
      (PosHdr.Complete,lcCustPO,Customer.BtName,OrdHdr.Order,;
       SUBSTR(PosLn.Style,1,12),Style.Desc,ApVendor.cVenComp,PosHdr.PO,;
       OrdHdr.Entered,llPrint)
    llPrint = .F.   
  = lfCalculate()
  ENDSCAN
  *-- To get the trims.
  llPrint = .T.
  SELECT cTktBom
  =SEEK('I'+PosHdr.PO)
  SCAN REST WHILE cImtyp+CutTkt+Typ+Item+Iclr+mfgcode+Dyelot = 'I'+PosHdr.PO;
            FOR cCatgTyp = 'T'
            
    INSERT INTO (lcTmpFile);
        (dComplete,cCustPo,cBTName,cOrder,cStyMaj,cDesc,cVendor,cPO,dEntered,;
         lPrint) ;
    VALUES ;
      (PosHdr.Complete,lcCustPO,Customer.BtName,OrdHdr.Order,;
       SUBSTR(PosLn.Style,1,12),Style.Desc,ApVendor.cVenComp,PosHdr.PO,;
       OrdHdr.Entered,llPrint)
    llPrint = .F.  
    = lfCalculate()
  ENDSCAN
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfCalculate                   (C200168)
*! Developer : Mohamed Shokry
*! Date      : 02/20/2001
*! Purpose   : Calculate REQ.QTY,USEDQTY,EST.EXTD.COST,ACT.EXTD.COST fields.
*!*************************************************************
*! Called from : POWIPALE.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCalculate()
*!*************************************************************
FUNCTION lfCalculate
PRIVATE lnAlias

lnAlias = SELECT()
lcCPo = Cuttkt
STORE 0 TO lnReqQty,lnUsedQty,lnEstExCst,lnActExCst

SELECT BOMLINE
*Used index : CIMTYP+CTYPE+CTKTNO+LINENO+CBOMTYP+STYLE+SCLR+IIF(!(cCatGTyp$'MDP'),ITEM,MFGCODE)+ICLR+STR(RECNO(),7)
SEEK 'I'+'1'+lcCPo
lcForKey=IIF(EMPTY(CTKTBOM.Item),CTKTBOM.MfgCode,CTKTBOM.Item+CTKTBOM.IClr)

SUM (ItemQty),(ItemQty*UnitCost) REST ;
         WHILE CIMTYP+CTYPE+CTKTNO='I'+'1'+lcCPo ;
         FOR  IIF(!(cCatGTyp$'MDP'),ITEM,MFGCODE)+ICLR = lcForKey .AND. cBomTyp=CTKTBOM.Typ ;
         TO   lnReqQty,lnEstExCst

* Display estimated purchase price in base currency
lnEstExCst = IIF(CTKTBOM.Typ='1' .AND. SEEK(XPO,'POSHDR'),;
                 lnEstExCst/POSHDR.RATE,lnEstExCst)

SELECT BOMCOST
*New Used Inex CBOMTYPE+CIMTYP+CTKTNO+ITEM+ICLR+MFGCODE+CWARECODE+CDYELOT+CRSESSION+CISESSION                                          
=SEEK(CTKTBOM.Typ+'I'+lcCPo+CTKTBOM.Item+CtktBom.IClr+CTKTBOM.MfgCode)
SUM nTotQty,nTotCst REST WHILE cBomType+cIMtyp+cTktNo+Item+IClr+MfgCode = ;
                    CTKTBOM.Typ+'I'+lcCPo+CTKTBOM.Item+CtktBom.IClr+CTKTBOM.MfgCode ;
                    TO lnUsedQty,lnActExCst 

SELECT ((lcTmpFile))
REPLACE cItem     WITH cTktBom.Item ,;
        cIclr     WITH cTktBom.Iclr,;
        cItmDesc  WITH cTktBom.Desc,;
        nUntCost  WITH cTktBom.UntCost,;
        nReqQty   WITH lnReqQty ,;
        nUsedQty  WITH lnUsedQty ,;
        nEstCost  WITH lnEstExCst,;
        nActCost  WITH lnActExCst,;
        cType     WITH cTktBom.Typ,;
        cCatgTyp  WITH cTktBom.cCatgTyp
        

SELECT (lnAlias)
