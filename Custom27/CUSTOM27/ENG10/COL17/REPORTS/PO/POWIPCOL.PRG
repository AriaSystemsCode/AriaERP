****************************************************************************
*: Program file      : POWIPCOL.PRG   (C#200367)
*: Program desc.     : WORK IN PROGRESS REPORT
*: System            : Aria Apparel System (A27).
*: Module            : Style Purchase Order (PO)
*: Developer         : AHMED MAHER (AMH)
*: Date              : 07/09/2002
*:**************************************************************************
*:Modifications :
*:****************************************************************************************

*-- Create temp File
STORE '' TO lcPoNo
lcTmpFile = gfTempName()
CREATE CURSOR (lcTmpFile) (dComplete D,cStatus C(4),cAccount C(5),cOrder C(6),dProdDate D,;
                           cStyMaj C(12),cDesc C(20),cVendor C(8),cPO C(6),nTOTQty N(10),;
                           nValue N(13,2),lPrntTot L,cShipType C(6),cPriceCur C(3))
INDEX ON DTOS(dComplete)+ cPO + cOrder + cStyMaj  TAG (lcTmpFile) OF (lcTmpFile)

*-- To get the selected vendors if any
lcVendFile = ''
llVendUse = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendFile = laOGFxFlt[lnPosition,6]
  llVendUse = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
ENDIF

*-- To get the selected customer if any.
lcCustFile = ''
llCustUse = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcCustFile = laOGFxFlt[lnPosition,6]
  llCustUse = IIF(!EMPTY(lcCustFile) .AND. USED(lcCustFile) .AND. RECCOUNT(lcCustFile)>0,.T.,.F.)
ENDIF

IF llVendUse
  =lfDataByVn()
ELSE
  IF llCustUse
    =lfDataByCs()
  ELSE
    =lfDataByAl()
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

DO gfDispRe WITH "POWIPCOL"

GOTO TOP
*--- Start printing report

SET DEVICE TO SCREEN

****************************************************************************
*! PROG      : lfDataByCs.PRG   (C#200367)
*! DESC      : function To collect data in the temporary file
*! Developer : AMHMED MAHER (AMH)
*! Date      : 07/09/2002
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
LOCATE
SCAN
  lcCustomer = Account
  IF SEEK(lcCustomer,'OrdHdr')
    SELECT OrdHdr
    SCAN REST WHILE account+cordtype+order = lcCustomer
      IF SEEK('2'+OrdHdr.Order,'CutPick')
        lcPoNo = CutPick.cTktNo
        IF SEEK('P'+lcPoNo,'PosHdr') .AND. PosHdr.Status # 'X'
          =lfGetPoLin('C')
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
*! PROG      : lfDataByAl.PRG   (C#200367)
*! DESC      : function To collect all lines from posln
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/09/2002
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

SELECT PosHdr
SCAN FOR Status # 'X'
  lcPoNo = PosHdr.PO
  =lfGetPoLin('V')
ENDSCAN

****************************************************************************
*! PROG      : lfDataByVn.PRG   (C#200367)
*! DESC      : function To scan for the selected vendors
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/09/2002
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
FUNCTION lfDataByVn

SELECT (lcVendFile)
LOCATE
SCAN
  lcVendCode = cVendCode
  IF SEEK(lcVendCode,'POSHDR')
    SELECT PosHdr
    SCAN REST WHILE Vendor+cStyType+PO = lcVendCode FOR Status # 'X'
      lcPoNo = PosHdr.PO
      =lfGetPoLin('V')
    ENDSCAN
  ENDIF
ENDSCAN

****************************************************************************
*! PROG      : lfReltdDat.PRG   (C#200367)
*! DESC      : function To get the related data from the style,cutpick,apvendor,
*!           : ordhdr,ordline and insert a new record in the temporary file
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/09/2002
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

lnAlias  = SELECT(0)
llReturn = .T.
=SEEK(PosLn.Style,'Style')
=SEEK('I'+POSHDR.PO,'CTKTBOM')
IF lcType = 'V'
  =SEEK('2'+PosHdr.PO+STR(PosLn.LineNo,6),'CutPick')
  =SEEK('O'+CutPick.Order,'OrdHdr')

  IF llCustUse .AND. !SEEK(OrdHdr.Account,lcCustFile)
    llReturn = .F.
    SELECT(lnAlias)
    RETURN llReturn
  ENDIF
ENDIF

INSERT INTO (lcTmpFile);
      (dComplete,cStatus,cAccount,cOrder,cStyMaj,cDesc,cVendor,cPO,dProdDate,cShipType,cPriceCur);
VALUES ;
      (PosHdr.Complete,lfGetStats(),ORDHDR.ACCOUNT,OrdHdr.Order,;
       SUBSTR(PosLn.Style,1,12),Style.Desc,POSHDR.VENDOR,PosHdr.PO,CTKTBOM.DATE,lfGetShip(),POSHDR.CPRICECUR)

SELECT(lnAlias)
RETURN llReturn

*!*************************************************************
*! Name      : lfsrvVen
*! Developer : AHMED MAHER
*! Date      : 07/09/2002
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
*! PROG      : lfGetPoLin.PRG   (C#200367)
*! DESC      : function To collect lines from posln
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/09/2002
****************************************************************************
FUNCTION lfGetPoLin
PARAMETERS lcParam

PRIVATE lnAlias
lnAlias = SELECT(0)
STORE 0 TO lnTotQty,lnValue
llUpdatRec = .F.

*-- To get the related data from the style,cutpick,apvendor,ordhdr,ordline
*-- and insert a new record in the temporary file
IF SEEK('P'+lcPoNo,'PosLn') AND lfReltdDat(lcParam)
  lnValue = POSLN.NCOST1
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
  
  SELECT (lcTmpFile)
  REPLACE nValue WITH lnValue
  IF nTotQty = 0
    DELETE
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfGetStats
*! Developer : AHMED MAHER
*! Date      : 07/10/2002
*! Purpose   : Get the status of PO.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetStats()
*!*************************************************************
FUNCTION lfGetStats

PRIVATE lcStatus
lcStatus = ''
DO CASE
  CASE POSHDR.STATUS = 'H'
    lcStatus = 'WARN'
  CASE POSHDR.STATUS $ 'OA'
    lcStatus = 'WORK'
  CASE POSHDR.STATUS = 'C'
    lcStatus = 'COMP'
ENDCASE
RETURN lcStatus
*-- End Of lfGetStats.

*!*************************************************************
*! Name      : lfGetShip
*! Developer : AHMED MAHER
*! Date      : 07/10/2002
*! Purpose   : Get the Ship Via Desc. of PO.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetShip()
*!*************************************************************
FUNCTION lfGetShip

PRIVATE lcShip
lcShip = ''
lcShip = LEFT(gfCodDes(POSHDR.SHIPVIA,'SHIPVIA   '),6)
RETURN lcShip
*-- End Of lfGetShip.