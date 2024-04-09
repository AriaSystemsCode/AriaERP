*:****************************************************************
*: Program file  : POSTYPT.PRG [Custom Report For Dare To Bare]
*: Program desc. : Critical Path Report.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchase Order  (PO)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 01/26/2004
*: Tracking Job Number : C#037345,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : lfvObjRng  ,  lfvPbOk  , lfClearRed
*:               :            : lfFilOpratn, lfCreatTmp, lfCollect
*:               :            : lfGetOrdQty.
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#037345,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- gcCompName : Variable hold the company name.
*-- lcSeleBy   : Variable hold select by and print it into the FRX.
*-- lcRpOprOrd : Variable hold select by into the option Grid.
*-- lcPrntOprt : Variable hold the current Operation.
*-- lcRpOprSt  : Variable hold the operation status.
*-- lcOprtitel : Variable hold the operation titel.

*-- start Declaration Variables.
STORE '' TO lcPrntOprt , lcOprtitel
*-- End Declaration Variables

*-- get the title for select by.
IF lcRpOprOrd = 'R'
  lcSeleBy = 'Select by Sales Order'
ELSE
  lcSeleBy = 'Select by Operation'
  *-- Get the operation titel.
ENDIF

DO CASE
  CASE lcRpOprSt  = 'O'
    lcOprtitel    = 'Unapproved'
  CASE lcRpOprSt  = 'A'
    lcOprtitel    = 'Approved'
  CASE lcRpOprSt  = 'P'
    lcOprtitel    = 'Approved Late'  
  CASE lcRpOprSt  = 'L'
    lcOprtitel    = 'All'
ENDCASE

IF lcRpOprOrd = 'R'
  lcPrntOprt = 'All'
ELSE
  lcPrntOprt = ALLTRIM(lcRpOprt)
ENDIF

*-- Print the last form in case
llPrntOpr = .F.
IF lcRpOprOrd = 'R'
  llPrntOpr = .T.
ENDIF




*-- Function To create Temp File to print from
=lfCreatTmp()

*-- Function To Collect Data
=lfCollect()


SELECT (lcMainTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpName')

*-- End OF Code
*:*************************************************************
*: Name      : lfStrstOrd
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Order In Range
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       :  None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : True.
*:*************************************************************
*: Example     : = lfStrstOrd()
*:*************************************************************
*
FUNCTION lfStrstOrd
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'

    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]

    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
ENDCASE

*-- End Of lfStrstOrd.
*:*************************************************************
*: Name      : lfvObjRng
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Showes range screen
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvObjRng()
*:*************************************************************
FUNCTION lfvObjRng
PARAMETERS lcObj
PRIVATE ldFrom , ldTo


*-- CASE "APPROVED" ---> 'Operation Approved date'
*-- CASE "REQUIRED" ---> 'Operation Required date'

DO CASE 
  CASE UPPER(lcObj) = "APPROVED"
    lcCall = "APPROVED"
    ldFrom = ldRPFApp
    ldTo   = ldRPTApp

  CASE UPPER(lcObj) = "REQUIRED"
    lcCall = "REQUIRED"
    ldFrom = ldRPFReq
    ldTo   = ldRPTReq

ENDCASE

DO CASE 
  CASE UPPER(lcObj) = "APPROVED"
    lcTitle = "Approved date"

  CASE UPPER(lcObj) = "REQUIRED"
    lcTitle = "Required date"

ENDCASE

DO DateRng.Spx

DO CASE 
  CASE UPPER(lcObj) = "APPROVED"
    STORE ldFrom TO ldRPFApp
    STORE ldTo   TO ldRPTApp

  CASE UPPER(lcObj) = "REQUIRED"
    STORE ldFrom TO ldRPFReq
    STORE ldTo   TO ldRPTReq
ENDCASE


*-- End of lfvObjRng.
*:*************************************************************
*: Name      : lfvPbOk
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Validate date range screen's OK button
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvPbOk()
*:*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE

  DO CASE
    CASE lcCall = "APPROVED"
      ldRPFApp = ldFrom
      ldRPTApp = ldTo

    CASE lcCall = "REQUIRED"
      ldRPFReq = ldFrom
      ldRPTReq = ldTo
  ENDCASE
  CLEAR READ
ENDIF

*-- End of lfvPbOk.
*:*************************************************************
*: Name      : lfClearRed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Function to clear read the option grid.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfClearRed()
*:*************************************************************
*:
FUNCTION lfClearRed

CLEAR READ
*-- End OF lfClearRed
*:*************************************************************
*: Name      : lfFilOpratn
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Function to fill the operation array.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFilOpratn()
*:*************************************************************
*:
FUNCTION lfFilOpratn

llOpenUser = gfOpenFile(gcDataDir+'STYCRPLN',gcDataDir+'STYCRPLN','SH')

DIMENSION laRpOprtnS[1,1] , laRpOprtnt[1,1]

STORE '' TO laRpOprtnS,laRpOprtnT

SELECT DISTINCT cCrpthOpr FROM stycrpln INTO ARRAY laRpOprtnS
lnAddLen = ALEN(laRpOprtnS)
DIMENSION laRpOprtnS[lnAddLen+1,1]
= AINS(laRpOprtnS, lnAddLen+1)
laRpOprtnS[lnAddLen+1,1] = 'All'

DIMENSION laRpOprtnt[lnAddLen+1,1]
FOR  I = 1 TO lnAddLen+1
  laRpOprtnt[I,1] = laRpOprtnS[I,1]
ENDFOR

*-- End OF lfFilOpratn
*:*************************************************************
*: Name      : lfCreatTmp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/26/2004
*: Purpose   : Create Temp File
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfCreatTmp()
*:*************************************************************
*:
FUNCTION lfCreatTmp
PRIVATE lnOld_Als

lnOld_Als = SELECT(0)
SELECT STYCRPLN
=AFIELDS(laFileStru)

lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+3,4]
laFileStru[lnFileStru+01,1] = 'cCode_No'
laFileStru[lnFileStru+01,2] = 'C'
laFileStru[lnFileStru+01,3] = 6
laFileStru[lnFileStru+01,4] = 0

laFileStru[lnFileStru+02,1] = 'nDays'
laFileStru[lnFileStru+02,2] = 'N'
laFileStru[lnFileStru+02,3] = 6
laFileStru[lnFileStru+02,4] = 0


laFileStru[lnFileStru+03,1] = 'nOrdQty'
laFileStru[lnFileStru+03,2] = 'N'
laFileStru[lnFileStru+03,3] = 12
laFileStru[lnFileStru+03,4] = 0


*-- Create an index.

DECLARE laIndex[1,2]
laIndex[1,1] = 'cCode_No+Po+DTOS(drequired)'
laIndex[1,2] = lcMainTemp

=gfCrtTmp(lcMainTemp,@laFileStru,@laIndex)

SELECT(lnOld_Als)

*-- End OF lfCreatTmp
*:*************************************************************
*: Name      : lfCollect
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Collect Data to Print.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect
PRIVATE lnOld_Als , lcScanExpr , lcOld_Po
STORE '' TO lcScanExpr , lcOld_Po
STORE 0 TO lnOldQty

lcMjrPct  = gfItemMask('PM')
lnMajorLen = LEN(lcMjrPct)

lnOld_Als = SELECT(0)
IF lcRpOprOrd = 'P' .AND. !(lcRpOprt = 'All')
  lcRpExp = lcRpExp +' .AND. ALLTRIM(ccrpthopr) = ALLTRIM(lcRpOprt)'
ENDIF

*-- Add expration depend on the operation status.
DO CASE
  CASE lcRpOprSt = 'O'
    lcOprExpr    = ' !EMPT(Dapproved)'
  CASE lcRpOprSt = 'A'
    lcOprExpr    = ' EMPT(Dapproved) '

    IF !(EMPTY(ldRPFApp) .AND. EMPTY(ldRPFApp))
      lcOprExpr    = lcOprExpr    + ' .OR. !BETWEEN(Dapproved,ldRPFApp,ldRPTApp)'
    ENDIF
    
  CASE lcRpOprSt = 'P'
    lcOprExpr    = ' Dapproved <= Drequired '
    IF !(EMPTY(ldRPFApp) .AND. EMPTY(ldRPFApp))
      lcOprExpr    = lcOprExpr    + ' .OR. !BETWEEN(Dapproved,ldRPFApp,ldRPTApp)'
    ENDIF
  OTHERWISE
    lcOprExpr    = '.F.'
ENDCASE

IF !(lcRpOprOrd = 'R') .AND.  !(lcRpOprt= 'All')
 lcRpExp = lcRpExp + " .AND.  ALLTRIM(cCrpthOpr) = ALLTRIM(lcRpOprt)"
ENDIF

lcReqDtExp = ".F."
IF  !(EMPTY(ldRPFReq) .AND. EMPTY(ldRPTReq))
  lcReqDtExp = "!BETWEEN(Drequired,ldRPFReq,ldRPTReq)"
ENDIF

SELECT STYCRPLN
SET RELATION TO LEFT(Style,lnMajorLen) INTO STYLE
SET RELATION TO 'O'+PO                 INTO ORDHDR ADDITIVE

SCAN FOR &lcRpExp .AND. Ordhdr.Status $ 'OH'
  WAIT WINDOW 'Collecting data for style # :'+ Style + ' ... please wait.' NOWAIT
  IF &lcOprExpr .OR. &lcReqDtExp
    LOOP
  ENDIF
  
  SCATTER MEMVAR MEMO
  SELECT (lcMainTemp)
  
  *-- get the order qty.
  
  m.nOrdQty = 0
  IF lcOld_po = M.Po
    m.nOrdQty = lnOldQty
  ELSE
    lcOld_Po  = M.Po
    lnOldQty  = lfGetOrdQty()
    m.nOrdQty = lnOldQty
  ENDIF
  
  *-- Get the Days O/Due.
  M.nDays = 0
  DO CASE
    CASE EMPTY(M.Drequired)
      M.nDays = 0    
    CASE !EMPTY(M.Dapproved) .AND. !EMPTY(M.Drequired)
        M.nDays = M.Dapproved - M.Drequired
    CASE EMPTY(M.Dapproved)  .AND. !EMPTY(M.Drequired)
        M.nDays = gdSysdate - M.Drequired
  ENDCASE
  M.cCode_No = Style.cDivision
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT STYCRPLN
SET RELATION TO
SELECT(lnOld_Als)
*-- End OF lfCollect
*:*************************************************************
*: Name      : lfGetOrdQty
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 12/31/2003
*: Purpose   : Get the order line Qty.
*:*************************************************************
*: Calls     : 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfCollect()
*:*************************************************************
*:
FUNCTION lfGetOrdQty
PRIVATE lnOldAls , lnRetQty
STORE 0 TO lnCOrdtQty , lnRetQty , lnOrdtQty
 = 0
lnOldAls = SELECT(0)
SELECT ORDLINE
IF SEEK('O'+M.Po)
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+M.Po ;
    FOR LEFT(Style,lnMajorLen) = LEFT(M.Style,lnMajorLen)
    lnOrdtQty = lnOrdtQty + TotBook
  ENDSCAN
ENDIF

SELECT ordcanln
IF SEEK('O'+M.Po)
  SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+M.Po ;
    FOR LEFT(Style,lnMajorLen) = LEFT(M.Style,lnMajorLen)
    lnCOrdtQty = lnCOrdtQty + TotQty
  ENDSCAN
ENDIF

lnRetQty = lnOrdtQty - lnCOrdtQty

SELECT(lnOldAls)
RETURN lnRetQty

*-- End OF lfGetOrdQty
*:*************************************************************