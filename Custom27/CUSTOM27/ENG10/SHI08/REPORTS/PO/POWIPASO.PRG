*:***************************************************************************
*: Program file  : POWIPASO.PRG
*: Program desc. : CUSTOMIZED STYLE PURCHASE ORDER OPERATION VALUES IN WIP REPORT FOR SHIO8
*: Date          : 12/24/2002
*: System        : Aria Advantage Series.
*: Module        : STYLE PURCHASE ORDER (PO)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C200446
*:
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO POWIPASO
*:***************************************************************************
*: Modification:
*****************************************************************************

*-- Create temp File
STORE '' TO lcPoNo, lcCustPo

*--The main functions of the report.
=lfCreatTmp()

*-- To get the selected vendors if any
lcVendFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVendFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcVendFile) AND USED(lcVendFile)
    SELECT (lcVendFile)
    LOCATE
  ENDIF
ENDIF

*-- To get the selected customer if any.
lcCustFile = ''
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT'),1)
IF lnPosition > 0
  lcCustFile = laOGFxFlt[lnPosition,6]
  IF !EMPTY(lcCustFile) AND USED(lcCustFile)
    SELECT (lcCustFile)
    LOCATE
  ENDIF
ENDIF

*--Section collect the data.
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

*--Print the Summery Report.
DO gfDispRe WITH EVALUATE('lcRpName')
WAIT CLEAR
SET DEVICE TO SCREEN

*--Call the function of deleting the temprory files.
=lfBasToClr(lcTmpFile , 'F')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

CREATE CURSOR (lcTmpFile) ;
     ( dComplete D    , cCustPo C(15) , cBTName C(30) , cOrder C(6) , dEntered D    ,;
       cStyMaj C(12)  , cDesc C(20)   , cVendor C(30) , cPO C(6)    , nTOTQty N(10) ,;
       nValue N(13,2) , lPrntTot L )
INDEX ON DTOS(dComplete)+ cPO + cOrder + cStyMaj  TAG (lcTmpFile) OF (lcTmpFile)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfDataByCs
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To collect data in the temporary file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDataByCs()
*!*************************************************************
FUNCTION lfDataByCs
PARAMETERS lcParam
PRIVATE lcOldOrd,lcCustOrd ,lcPoOrd

SELECT ORDHDR
lcCustOrd = SET('ORDER')
SET ORDER TO Ordacct

SELECT CUTPICK
lcOldOrd = SET('ORDER')
SET ORDER TO Cutord

SELECT POSHDR
lcPoOrd = SET('ORDER')
SET ORDER TO POSHDR

SELECT (lcCustFile)
LOCATE
SCAN
  WAIT WINDOW 'Selecting Records For Account...' + ACCOUNT NOWAIT
  lcCustomer = Account
  IF SEEK(lcCustomer,'OrdHdr')
    SELECT OrdHdr
    SCAN REST WHILE account+cordtype+order = lcCustomer
      IF SEEK('2'+OrdHdr.Order,'CutPick')
        lcPoNo = CutPick.cTktNo
        IF SEEK('P'+lcPoNo,'POSHDR') AND !INLIST(POSHDR.Status,'X','H')
          lcPoNo = POSHDR.PO
          =lfGetPoLin()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

SELECT ORDHDR
SET ORDER TO &lcCustOrd

SELECT CUTPICK
SET ORDER TO &lcOldOrd

SELECT POSHDR
SET ORDER TO &lcPoOrd

*--End of lfDataByCs.
*!*************************************************************
*! Name      : lfDataByAl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To collect all lines from POSLN.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDataByAl()
*!*************************************************************
FUNCTION lfDataByAl
PARAMETERS lcParam

SELECT POSHDR
SCAN FOR !INLIST(Status,'X','H')
  WAIT WINDOW 'Selecting Records For PO...' + POSHDR.PO NOWAIT
  lcPoNo = POSHDR.PO
  =lfGetPoLin()
ENDSCAN

*--End of lfDataByAl.
*!*************************************************************
*! Name      : lfDataByVn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To scan for the selected vendors.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfDataByVn()
*!*************************************************************
FUNCTION lfDataByVn
PARAMETERS lcParam

SELECT (lcVendFile)
LOCATE

*-- If there are selected vendors
IF !EOF()
  SCAN
    WAIT WINDOW 'Selecting Records For Vendor...' + cVendCode NOWAIT
    lcVendCode = cVendCode
    IF SEEK(lcVendCode,'POSHDR')
      SELECT POSHDR
      SCAN REST WHILE VENDOR + CSTYTYPE + PO = lcVendCode FOR !INLIST(Status,'X','H')
        lcPoNo = POSHDR.PO
        =lfGetPoLin()
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF

*--End of lfDataByVn.
*!*************************************************************
*! Name      : lfReltdDat
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To get data from Style,cutpick,apvendor,ordhdr,ordline.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfReltdDat()
*!*************************************************************
FUNCTION lfReltdDat
PARAMETERS lcType

PRIVATE llReturn, lnAlias
lcCustPO = ''
lnAlias  = SELECT()
llReturn = .T.
=SEEK(POSLN.STYLE,'STYLE')
=SEEK(POSHDR.VENDOR,'APVENDOR')
IF lcType = 'V'
  =SEEK('2' + POSHDR.PO + STR(POSLN.LINENO,6),'CUTPICK')
  =SEEK('O' + CUTPICK.ORDER,'ORDHDR')

  IF !EMPTY(lcCustFile) AND USED(lcCustFile)
    SELECT(lcCustFile)
    LOCATE
    IF !SEEK(ORDHDR.ACCOUNT,lcCustFile)
      llReturn = .F.
      SELECT(lnAlias)
      RETURN llReturn
    ENDIF
  ENDIF
ENDIF

IF ORDHDR.MULTI = 'Y'
  =SEEK('S'+ ORDHDR.ACCOUNT,'CUSTOMER')
ELSE
  =SEEK('M'+ ORDHDR.ACCOUNT,'CUSTOMER')
ENDIF

IF !(ORDHDR.MULTIPO)
  lcCustPO = ORDHDR.CUSTPO
ELSE
  =SEEK('O' + CUTPICK.ORDER + PADL(CUTPICK.CTKTLINENO,6,' '),'ORDLINE')
  lcCustPO = ORDLINE.CUSTPO
ENDIF

INSERT INTO (lcTmpFile);
      (dComplete,cCustPo,cBTName,cOrder,cStyMaj,cDesc,cVendor,cPO,dEntered);
VALUES ;
      (POSHDR.Complete,lcCustPO,Customer.BtName,OrdHdr.Order,;
       SUBSTR(POSLN.STYLE,1,12),STYLE.Desc,ApVendor.cVenComp,POSHDR.PO,OrdHdr.Entered)

SELECT(lnAlias)
RETURN llReturn

*--End of lfReltdDat.
*!*************************************************************
*! Name      : lfGetPoLin
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To collect lines from POSLN.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetPoLin()
*!*************************************************************
FUNCTION lfGetPoLin
PRIVATE lnAlias

lnAlias = SELECT()
STORE 0 TO lnTotQty,lnValue
llUpdatRec = .F.
llRecevLn  = .F.              && Check if there is a received value or not

*-- To get the related data from the style,cutpick,apvendor,ordhdr,ordline
*-- and insert a new record in the temporary file
IF SEEK('P'+lcPoNo,'POSLN') AND lfReltdDat(lcParam)
  lnValue = lfGetValue(lcPoNo)
  lnTotQty = 0

  SELECT POSLN
  DO WHILE CSTYTYPE + PO + STYLE + STR(lineno,6) + TRANCD = POSHDR.CSTYTYPE + POSHDR.PO

    llUpdatRec = .T.
    STORE 0 TO lnOrgQty1,lnOrgQty2,lnOrgQty3,lnOrgQty4,lnOrgQty5,lnOrgQty6,;
               lnOrgQty7,lnOrgQty8

    STORE 0 TO lnRecQty1,lnRecQty2,lnRecQty3,lnRecQty4,lnRecQty5,lnRecQty6,;
               lnRecQty7,lnRecQty8

    lcStyKey = CSTYTYPE + PO + POSLN.STYLE
    SCAN REST WHILE CSTYTYPE + PO + POSLN.STYLE + STR(lineno,6) + TRANCD = lcStyKey
      SELECT (lcTmpFile)

      *-- Get the original qty
      IF POSLN.TRANCD = '1'
        FOR lnCounter = 1 TO 8
          lcCounter = STR(lnCounter,1)
          lnOrgQty&lcCounter = lnOrgQty&lcCounter + POSLN.Qty&lcCounter
        ENDFOR
        lnTotQty = lnTotQty + POSLN.TotQty
      ELSE

        IF !EMPTY(ldRpCutOFF)
          IF POSLN.Date <= ldRpCutOFF
            IF POSLN.TRANCD <>'3'
              llRecevLn = .T.
              FOR lnCounter = 1 TO 8
                lcCounter = STR(lnCounter,1)
                lnRecQty&lcCounter = lnRecQty&lcCounter + POSLN.Qty&lcCounter
              ENDFOR
            ENDIF
          ENDIF
        ELSE
          IF POSLN.TRANCD <>'3'
            llRecevLn = .T.
            FOR lnCounter = 1 TO 8
              lcCounter = STR(lnCounter,1)
              lnRecQty&lcCounter = lnRecQty&lcCounter + POSLN.Qty&lcCounter
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
    SELECT POSLN
  ENDDO

  lnNewQty = 0
  IF llRpWipOp
    &&--lcOrdTkBom      >> Hold the old tag of the file CTKTBOM
    &&--lcHldOpr        >> Hold the operation name to not dublicated it in the scaning.
    PRIVATE lcOrdTkBom , lcHldOpr

    SELECT CTKTBOM
    lcOrdTkBom = ORDER()
    SET ORDER TO TAG CTKTYP

    SELECT MFGOPRDT
    IF SEEK("I"+POSHDR.PO)
      lcHldOpr = SPACE(0)
      SCAN REST WHILE Cimtyp + Ctktno + COPRCODE + Clotno + TRANCD = "I" + POSHDR.PO ;
                FOR TRANCD $ "23"
        IF lcHldOpr # MFGOPRDT.COPRCODE
          IF SEEK("I"+POSHDR.PO+SPACE(25)+MFGOPRDT.COPRCODE+MFGOPRDT.cDyelot,'cTktBom')
            IF llRecevLn
              lnNewQty = lnNewQty + (&lcTmpFile..nTotQty * cTktBom.UntCost)
            ELSE
              lnNewQty = lnNewQty + (MFGOPRDT.nLotTotQty * cTktBom.UntCost)
            ENDIF
          ENDIF
          lcHldOpr = MFGOPRDT.COPRCODE
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF

  *--Get the original value of the variable.
  llRecevLn  = .F.

  lnTotVal = IIF(lnTotQty <> 0, lnValue / lnTotQty,0)
  SELECT (lcTmpFile)
  REPLACE nValue WITH (lnTotVal * nTotQty) + lnNewQty

  IF nTotQty = 0
    DELETE
    llUpdatRec = .F.
  ENDIF

  IF llRpWipOp
    SELECT CTKTBOM
    SET ORDER TO &lcOrdTkBom
  ENDIF
ENDIF

SELECT (lnAlias)

*--End of lfGetPoLin.
*!*************************************************************
*! Name      : lfGetValue
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Function To Get Value.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetValue()
*!*************************************************************
FUNCTION lfGetValue
PARAMETERS lcPo
PRIVATE lnAlias, lnVal

lnAlias = SELECT()
lnVal   = 0
IF SEEK(lcPo,'MATINVJL')
  SELECT MATINVJL
  SUM REST (nUnitCost*nissued) - (nunitcost * nreceived) TO lnVal ;
  WHILE cTran+COPRCODE+cLotNo+cTranType+cFabric+cColor+cWareCode = lcPo;
  FOR CTranType = "4" AND IIF(EMPTY(ldRpCutOFF) , .T. , MATINVJL.dPostDate <= ldRpCutOFF )
ENDIF
SELECT(lnAlias)
RETURN lnVal

*--End of lfGetValue.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*!*************************************************************
*! Name      : lfsrvVen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
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
*!*************************************************************
*! Name      : lfsrvCust
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 12/24/2002
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
FUNCTION lfsrvCust
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  GO TOP IN CUSTOMER
ENDIF

*-- End Of lfsrvCust.