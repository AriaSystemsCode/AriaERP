*:***************************************************************************
*: Program file  : MADPODT
*: Program desc. : DORBY Material PURCHASE ORDER DETAIL
*: Date          : 08/08/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : Hossam El Etreby [HDM]
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: DUE TO : C101601
*:***************************************************************************
*: Example : DO MADPODT
*:***************************************************************************

*-- Define global report vars.
STORE 0  TO lnTotUse , lnAmt , lnOrdAmt , lnTotOrd , lnTotRec , lnTotDam , lnTotCan , lnOpn , lnTotOpn , lnOpnAmt , lnRecAmt , lnCanAmt, lnDamAmt
STORE 0  TO lnGrOrd , lnGrOrdAmt , lnGrRec , lnGrRecAmt , lnGrCan , lnGrCanAmt , lnGrDam , lnGrDamAmt , lnGrOpn , lnGrOpnAmt
STORE '' TO lcPoString
*-- Establish relations
SELECT POFHDR
SET RELATION TO Pofhdr.cmattype+ Pofhdr.pomat INTO Pofln ADDITIVE

SELECT POFLN
SET RELATION TO Pofln.fabric+ Pofln.color INTO Fabric ADDITIVE

SET RELATION TO 'M'+ Pofln.pomat+ Pofln.fabric+ Pofln.color INTO Dfabric ADDITIVE

SELECT POFHDR
set skip to pofln
GO TOP

*lcRpExp = IIF(EMPTY(lcRpExp), "POFLN.TRANCD = '1'" , lcRpExp + " AND POFLN.TRANCD='1'" )
lcRpExp = IIF(EMPTY(lcRpExp), "!EOF('POFHDR')" ,lcRpExp + " AND !EOF('POFHDR')" )
*-- Call the Report
*DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
DO gfDispRe WITH 'MADPODT.FRX' , 'FOR ' + lcRpExp


*!*************************************************************
*! Name      : lfvVen
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : Validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVen()
*!*************************************************************

FUNCTION lfvVen
PRIVATE lcVenFld,lcVendor,lcSelFile,lcApvenTag

lcVenFld  = VARREAD()
lcVendor  = EVAL(lcVenFld) 

lnSelFile = SELECT(0)

SELECT APVENDOR
lcApvenTag  = ORDER('APVENDOR')

SET ORDER TO TAG VENCODE IN APVENDOR

IF !EMPTY(lcVendor) .AND.('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
  
&lcVenFld = lcVendor

SET ORDER TO  lcApvenTag
 
SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfvPo
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : VALIDATE THE PURCHASE ORDER MATERIAL
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPo()
*!*************************************************************

FUNCTION lfvPo
PRIVATE lcPoFld,lcPorder,lcSelFile,lcPOTag

lcPoFld   = VARREAD()
lcPomat  = EVAL(lcPoFld)

lnSelFile = SELECT(0)

SELECT POFHDR
lcPOTag   = ORDER('POFHDR')

SET ORDER TO TAG POFHDR IN POFHDR

IF !EMPTY(lcPomat) .AND. ('?' $ lcPomat .OR. !SEEK('P'+lcPomat , 'POFHDR'))
  =POFBROW('P','',@lcPomat)
ENDIF
 
&lcPoFld  = lcPomat

SET ORDER TO lcPOTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvFabric
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : VALIDATE THE FABRIC
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************

FUNCTION lfvFabric

PRIVATE lcFabld,lcFabrc,lnSelFile,lcFaTag 

lcFabld   = VARREAD()
lcFabrc  = EVAL(lcFabld)

lnSelFile = SELECT(0)

SELECT FABRIC
lcFaTag   = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabrc) .AND. ('?' $ lcFabrc .OR. !SEEK(lcFabrc , 'FABRIC'))
  =FABROW (@lcFabrc,'*')
ENDIF
 
&lcFabld = lcFabrc

SET ORDER TO lcFaTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*lcFormName = 'MADPODT.FRX'
*lcRpName    = 'MADPODT.FRX'
llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

*!*************************************************************
*! Name      : lfSRVPo
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POFHDR
    SET ORDER TO TAG POFHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POFHDR

  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO 

ENDCASE
*-- end of lfSRVPo
*!*************************************************************
*! Name      : lfTotUse
*! Developer : Hossam El Etreby [HDM]
*! Date      : 08/08/1999
*! Purpose   : Calculate lfTotUse variable for the FRX
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : MADPODT.FRX 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : lnTotUse
*!*************************************************************
*! Example   : =lfTotUse()
*!*************************************************************

FUNCTION lfTotUse

WAIT WINDOW 'Collecting data ... Vandor : ' + POFHDR.VENDOR +'PO# '+ POFHDR.POMAT NOWAIT
DO CASE
  CASE POFHDR.STATUS $ 'OB'
    lnTotUse = POFLN.NFABTOTQTY

  CASE POFHDR.STATUS = 'X'
    lnTotUse = 0

  CASE POFHDR.STATUS = 'C'
    lcKey = POFLN.cmattype + POFLN.pomat + POFLN.fabric + POFLN.color + '2'
    SELECT POFLN
    lnRecNo = RECNO()
    SUM POFLN.NFABTOTQTY TO lnTotUse ;
    FOR POFLN.cmattype + POFLN.pomat + POFLN.fabric + POFLN.color + POFLN.trancd = lcKey;
    AND POFLN.CFABGRADE = '1'
    GO lnRecNo
    SELECT POFHDR
ENDCASE
WAIT WINDOW 'Calculating subtotals for Vandor : ' + POFHDR.VENDOR NOWAIT


DO CASE
  CASE POFLN.TRANCD = '1'
    lnTotOrd = lnTotOrd + lnTotUse
    lnOrdAmt = lnOrdAmt + (lnTotUse * pofln.ncost1)
  
  CASE POFLN.TRANCD = '2'
    lnTotRec = lnTotRec + lnTotUse
    lnRecAmt = lnRecAmt + (lnTotUse * pofln.ncost1)
  
  CASE POFLN.TRANCD = '3'
    lnTotDam = lnTotDam + lnTotUse
    lnDamAmt = lnDamAmt +(lnTotUse * pofln.ncost1)
  
  CASE POFLN.TRANCD = '4'
    lnTotCan = lnTotCan + lnTotUse
    lnCanAmt = lnCanAmt + (lnTotUse * pofln.ncost1)
  
ENDCASE

IF !(POFHDR.POMAT $ lcPoString)
  lnTotOpn = lnTotOpn + pofhdr.npo_open
  lnOpnAmt = lnOpnAmt + (pofhdr.npo_open * POFLN.NCOST1)
  lcPoString = lcPoString + '-' + POFHDR.POMAT
ENDIF
RETURN .T.


FUNCTION lfEndGroup

lnGrOrd    = lnGrOrd    + lnTotOrd
lnGrOrdAmt = lnGrOrdAmt + lnOrdAmt
lnGrRec    = lnGrRec    + lnTotRec
lnGrRecAmt = lnGrRecAmt + lnRecAmt
lnGrCan    = lnGrCan    + lnTotCan
lnGrCanAmt = lnGrCanAmt + lnCanAmt
lnGrDam    = lnGrDam    + lnTotDam
lnGrDamAmt = lnGrDamAmt + lnDamAmt
lnGrOpn    = lnGrOpn    + lnTotOpn
lnGrOpnAmt = lnGrOpnAmt + lnOpnAmt


STORE 0  TO lnOrdAmt , lnTotOrd , lnTotRec , lnTotDam , lnTotCan , lnTotOpn , lnOpnAmt , lnRecAmt , lnCanAmt, lnDamAmt
