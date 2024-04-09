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
*:B604368,1 KHM 04/07/2001 Fix the bug of not calculating the Open Qty when
*:B604368,1                over receiving is done on sizes.
*:B605836,1 KHM 04/09/2002 Fix the bug of getting only the open POs.
*:B606329,1 KHM 07/31/2002 Fix the bug of print POs with 0 quantity. 
*:C200450,1 BWA 12/25/2002 Filtering the material inventory journal according to cut off date.
*:C200549,1 AMH 05/05/2003 Add option to print PO's with open quantities but no WIP value
*:C200549,1                also include the value of Style Components issued.
*:****************************************************************************************

*-- Create temp File
STORE '' TO lcPoNo, lcCustPo
*C200450,1 BWA 12/25/2002 (Begin) Commented this line and put it in the syrepuvr.
*lcTmpFile = gfTempName()
*C200450,1 BWA 12/25/2002 (End)

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
LOCATE
IF RECCOUNT() = 0 .OR. EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT
GOTO BOTT
REPLACE lPrntTot WITH .T.
LOCATE

*--Check if the summery or the detial report to print.
IF llRpDetal
  lcRpName = "POWIPALE"
ELSE
  lcRpName = "POWIPALP"
ENDIF
=gfCrtFrm(lcRpName,lcOGFormArr,llOGRefForm)
=lfRepPltFr(lcRpName)

SELECT(lcTmpFile)
LOCATE
DO gfDispRe WITH EVALUATE('lcRpName')
WAIT CLEAR
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
  WAIT WINDOW 'Selecting Records For Account...' + ACCOUNT NOWAIT
  lcCustomer = Account
  IF SEEK(lcCustomer,'OrdHdr')
    SELECT OrdHdr
    SCAN REST WHILE account+cordtype+order = lcCustomer
      IF SEEK('2'+OrdHdr.Order,'CutPick')
        lcPoNo = CutPick.cTktNo
        
        *B605836,1 KHM 04/09/2002 (Begin) Getting all the POs except "X" & "H"        
        *IF SEEK('P'+lcPoNo,'PosHdr') AND PosHdr.Status = 'O'
        
        *C200549,1 AMH Include all status except canceled [Start]
        *IF SEEK('P'+lcPoNo,'PosHdr') AND !INLIST(PosHdr.Status,'X','H')
        IF SEEK('P'+lcPoNo,'PosHdr') .AND. PosHdr.Status # 'X'
        *C200549,1 AMH [End]
        
        *B605836,1 KHM 04/09/2002 (End)
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
*B605836,1 KHM 04/09/2002 (Begin) Getting all the POs except "X" & "H"
*SCAN FOR Status =  "O"

*C200549,1 AMH Include all status except canceled [Start]
*SCAN FOR !INLIST(Status,'X','H')
SCAN FOR Status # 'X'
*C200549,1 AMH [End]

*B605836,1 KHM 04/09/2002 (End)
  WAIT WINDOW 'Selecting Records For PO...' + POSHDR.PO NOWAIT
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
    WAIT WINDOW 'Selecting Records For Vendor...' + cVendCode NOWAIT
    lcVendCode = cVendCode
    IF SEEK(lcVendCode,'POSHDR')        
      SELECT PosHdr        
      *B605836,1 KHM 04/09/2002 (Begin) Getting all the POs except "X" & "H"
      *SCAN REST WHILE Vendor+cStyType+PO = lcVendCode FOR Status $ "O"
      
      *C200549,1 AMH Include all status except canceled [Start]
      *SCAN REST WHILE Vendor+cStyType+PO = lcVendCode FOR !INLIST(Status,'X','H')
      SCAN REST WHILE Vendor+cStyType+PO = lcVendCode FOR Status # 'X'
      *C200549,1 AMH [End]
      
      *B605836,1 KHM 04/09/2002 (End)

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
=SEEK(PosLn.Style,'Style')
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
*:B604368,1 KHM 04/07/2001 Commented out by changing the name
FUNCTION lfGetPoLiX
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
    
    *B604368,1 KHM 04/07/2001 (Begin) Initializing the necessary variables
    *B604368,1                to hold the open qty.
    STORE 0 TO lnOpnQty1,lnOpnQty2,lnOpnQty3,lnOpnQty4,lnOpnQty5,lnOpnQty6,;
               lnOpnQty7,lnOpnQty8
    *B604368,1 KHM 04/07/2001 (End)
    
    lcStyKey = cstytype+PO+SUBSTR(Posln.Style,1,12)
    SCAN REST WHILE cstytype+po+Posln.style+STR(lineno,6)+trancd = lcStyKey
      SELECT (lcTmpFile)
      IF PosLn.TranCd='1'
        *REPLACE nTotQty WITH nTotQty + PosLn.TotQty
        *B604368,1 KHM 04/07/2001 (Begin) Getting the original qty for each size.
        FOR lnCounter = 1 TO 8
          lcCounter = STR(lnCounter,1)
          lnOrgQty&lcCounter = lnOrgQty&lcCounter + PosLn.Qty&lcCounter
        ENDFOR
        *B604368,1 KHM 04/07/2001 (End)

        lnTotQty = lnTotQty + PosLn.TotQty
      ELSE
      
        *:C200168,1 MHM 02/20/2001 add cut off date to get total with [start]
        *REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)
        IF !EMPTY(ldRpCutOFF) 
          IF PosLn.Date <= ldRpCutOFF

            *B604368,1 KHM 04/07/2001 (Begin) Getting the open Qty for each size.
            *REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)
            FOR lnCounter = 1 TO 8
              lcCounter = STR(lnCounter,1)
              REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',;
                               MIN(PosLn.Qty&lcCounter,lnOpnQty&lcCounter),;
																        0)
            ENDFOR
            *B604368,1 KHM 04/07/2001 (End)

          ENDIF  
        ELSE

          *B604368,1 KHM 04/07/2001 (Begin) Getting the open Qty for each size.
          *REPLACE nTotQty WITH nTotQty-IIF(PosLn.TranCd <>'3',PosLn.TotQty,0)
          IF PosLn.TranCd <>'3'
            FOR lnCounter = 1 TO 8
              lcCounter = STR(lnCounter,1)      
              lnOpnQty&lcCounter = lnOpnQty&lcCounter + MAX(lnOrgQty&lcCounter- PosLn.Qty&lcCounter,0)

            ENDFOR
          ENDIF  
          *B604368,1 KHM 04/07/2001 (End)
        ENDIF
        *:C200168,1 MHM 02/20/2001 [end]
      ENDIF                    
    ENDSCAN
  ENDDO
  
  SELECT (lcTmpFile)
  FOR lnCntr = 1 TO 8
    lcCntr = STR(lnCntr,1)
    REPLACE nTotQty WITH nTotQty+lnOpnQty&lcCntr 
  ENDFOR

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

  *C200450,1 BWA 12/25/2002 (Begin) Add the checking of the cutt of date.  
  *SUM REST (nUnitCost*nissued) - (nunitcost * nreceived) TO lnVal ;
  WHILE cTran+cOprCode+cLotNo+cTranType+cFabric+cColor+cWareCode = lcPo;
  FOR CTranType = "4"

  SUM REST (nUnitCost*nissued) - (nunitcost * nreceived) TO lnVal ;
  WHILE cTran+cOprCode+cLotNo+cTranType+cFabric+cColor+cWareCode = lcPo;
  FOR CTranType = "4" AND IIF(EMPTY(ldRpCutOFF) , .T. , MATINVJL.DPOSTDATE <= ldRpCutOFF )
  *C200450,1 BWA 12/25/2002 (End)
ENDIF

*C200549,1 AMH Include the value of Style Components [Start]
lnValSty = 0
IF SEEK(lcPo,'STYINVJL')
  SELECT STYINVJL
  SUM REST -NSTKVAL TO lnValSty ;
  WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = lcPo;
  FOR CTrType = "I" AND IIF(EMPTY(ldRpCutOFF) , .T. , STYINVJL.DTRDATE <= ldRpCutOFF )
ENDIF
lnVal = lnVal + lnValSty
*C200549,1 AMH [End]

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
  
  *C200549,1 AMH Include the value of Style Components [Start]
  llPrint = .T.
  SELECT cTktBom
  =SEEK('I'+PosHdr.PO)
  SCAN REST WHILE cImtyp+CutTkt+Typ+Item+Iclr+mfgcode+Dyelot = 'I'+PosHdr.PO;
            FOR cCatgTyp = 'S'
            
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
  *C200549,1 AMH [End]
  
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


****************************************************************************
*! PROG      : lfGetPoLin.PRG   (C#200156)
*! DESC      : function To collect lines from posln
*! Developer : Khalid Mohi El-Din
*! Date      : 04/07/2001 
****************************************************************************
*:B604368,1 KHM 04/07/2001 
****************************************************************************
FUNCTION lfGetPoLin
PRIVATE lnAlias

lnAlias = SELECT()
STORE 0 TO lnTotQty,lnValue
llUpdatRec = .F.

*-- To get the related data from the style,cutpick,apvendor,ordhdr,ordline
*-- and insert a new record in the temporary file
IF SEEK('P'+lcPoNo,'PosLn') AND lfReltdDat(lcParam)
  lnValue = lfGetValue(lcPoNo)  
 lnTotQty = 0

  SELECT PosLn
  DO WHILE cstytype+po+style+STR(lineno,6)+trancd = PosHdr.cStyType+PosHdr.PO 
  
    llUpdatRec = .T.
    
    STORE 0 TO lnOrgQty1,lnOrgQty2,lnOrgQty3,lnOrgQty4,lnOrgQty5,lnOrgQty6,;
               lnOrgQty7,lnOrgQty8

    STORE 0 TO lnRecQty1,lnRecQty2,lnRecQty3,lnRecQty4,lnRecQty5,lnRecQty6,;
               lnRecQty7,lnRecQty8

    lcStyKey = cstytype+PO+Posln.Style
    SCAN REST WHILE cstytype+po+Posln.style+STR(lineno,6)+trancd = lcStyKey
      SELECT (lcTmpFile)
      
      *-- Get the original qty
      IF PosLn.TranCd='1'
        FOR lnCounter = 1 TO 8
          lcCounter = STR(lnCounter,1)
          lnOrgQty&lcCounter = lnOrgQty&lcCounter + PosLn.Qty&lcCounter
        ENDFOR
        lnTotQty = lnTotQty + PosLn.TotQty
      ELSE
      
        IF !EMPTY(ldRpCutOFF) 
          IF PosLn.Date <= ldRpCutOFF

            IF PosLn.TranCd <>'3'
              FOR lnCounter = 1 TO 8
                lcCounter = STR(lnCounter,1)      
                lnRecQty&lcCounter = lnRecQty&lcCounter + PosLn.Qty&lcCounter
              ENDFOR
            ENDIF  
          ENDIF  

        ELSE
        
          IF PosLn.TranCd <>'3'
            FOR lnCounter = 1 TO 8
              lcCounter = STR(lnCounter,1)      
              lnRecQty&lcCounter = lnRecQty&lcCounter + PosLn.Qty&lcCounter
            ENDFOR
          ENDIF  
        ENDIF

      ENDIF                    
    ENDSCAN
  
    SELECT (lcTmpFile)
    FOR lnCntr = 1 TO 8
      lcCntr = STR(lnCntr,1)
      REPLACE nTotQty WITH nTotQty+MAX(lnOrgQty&lcCntr - lnRecQty&lcCntr,0)
    ENDFOR
    SELECT PosLn
  ENDDO
  
  lnTotVal = IIF(lnTotQty <> 0,lnValue / lnTotQty,0)
  SELECT (lcTmpFile)
  REPLACE nValue WITH lnTotVal * nTotQty
  
  *B606329,1 KHM 07/31/2002 (Begin) Delete the record if the Totqty is 0 and do not
  *B606329,1                get the detailed costing.
  *IF nTotQty = 0
  
  *C200549,1 AMH Print PO's with zero WIP value [Start]
  *IF nTotQty = 0 OR nValue = 0
  IF nTotQty = 0 .OR. IIF(llRpPrtZro,.F.,nValue = 0)
  *C200549,1 AMH [End]
  
    DELETE
    llUpdatRec = .F.
  ENDIF   
  *B606329,1 KHM 07/31/2002 (End)
  
ENDIF

IF llRpDetal AND llUpdatRec = .T.
  =lfCostItem()
ENDIF

SELECT (lnAlias)


*!*************************************************************
*! Name      : lfsrvCust
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/25/2002
*! Purpose   : Rise change Account flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvCust()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*C200450,1 BWA 12/25/2002 
*!*************************************************************
FUNCTION lfsrvCust
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN CUSTOMER
ENDIF

*-- End Of lfsrvCust.