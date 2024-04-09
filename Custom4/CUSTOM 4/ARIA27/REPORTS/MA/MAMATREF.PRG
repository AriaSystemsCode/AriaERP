*:**********************************************************************************************
*: Program file  : MAVMREF.PRG 
*: Program desc. : Vendor Item\Color Report
*: System        : Aria Advantage Series.
*: Module        : MA
*: Developer     : Adel Mohhamed El Gazzar (ADEL)
*: Date          : 08/21/02
*: Refer to      : C(102683)
*:**********************************************************************************************
*: Modifications :
*:B122673,1 NNA 05/15/2004 change the Vendmatl.Price field name to be (nFabCost) So I changed the 
*:B122673,1 NNA            Price filed in the FRX to Get Frem NFabcost Field
*:**********************************************************************************************
*B122673,1 NNA 05/15/2004 (Begin) Save the current [Set decimals] and set it to 3
PRIVATE lnSetDecim
lnSetDecim = SET('DECIMALS')
SET DECIMALS TO 3
*B122673,1 NNA (End)

IF llOgFltCh
   llOgFltCh = .F.
   IF USED(lcTmpDet)
     SELECT (lcTmpDet)
     ZAP
     =lfCollect()
   ENDIF  
ENDIF
*--
SELECT (lcTmpDet)
GO TOP
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*--Sort
IF lcRpSortBy = "V"
  SET ORDER TO MatCol
ELSE
  SET ORDER TO ITEM
ENDIF
SET RELATION TO 
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDI
SET RELATION TO VENDOR INTO VENDMATH     ADDI
lcCurrItem = " " 
*-- Start printing
DO gfDispRe WITH EVAL('lcRPForm')
SET DEVICE TO SCREEN

*B122673,1 NNA 05/15/2004 (Begin) Return to the Original [Set Decimals]
SET DECIMALS TO lnSetDecim
*B122673,1 NNA (End)

RETURN
*--End of program ALBULK

*!*************************************************************
*! Name      : lfvVendor
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVendor) .AND. ;
   ('?' $ lcRPVendor .OR. !SEEK(lcRPVendor , 'APVENDOR') OR ATC("M",ALLTRIM(APVENDOR.cVenSupTyp))=0)
  *=gfApVnBrow(@lcRPVendor)
  =gfApVnBrow(@lcRPVendor,.F.,'M')
  IF EMPTY(lcRPVendor)
    lcRPVendor = lcOldVal
  ENDIF
ENDIF
DO lpSuppHanl

*!*************************************************************
*! Name      : lfwOldVal
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Get fields and some of the Option grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
RETURN

*!*************************************************************
*! Name      : lfwRepWhen
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF !USED('VENDMATL')
  =gfOpenFile(gcDataDir+'VENDMATL','VenMat1','SH')
ENDIF
IF !USED('VENDMATH')
  =gfOpenFile(gcDataDir+'VENDMATH','Vendmath','SH')
ENDIF
IF !USED('POFLN')
  =gfOpenFile(gcDataDir+'POFLN','Poflnf','SH')
ENDIF
DO lpSuppHanl

*--Create teh temp file
IF !EMPTY(lcTmpDet) AND !USED(lcTmpDet)
  SELECT VENDMATL  
  =AFIELDS(laFileStru)  
  lnArrLen = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnArrLen+1,4]
  *--Add the Print field.
  laFileStru[lnArrLen+1,1] = 'Print'
  laFileStru[lnArrLen+1,2] = 'L'
  laFileStru[lnArrLen+1,3] = ''
  laFileStru[lnArrLen+1,4] = ''
  
  DECLARE laIndex[3,2]
  laIndex[1,1] = 'Vendor+fabric+color+cvenfab+cvencolr'
  laIndex[1,2] = 'MatCol'
  laIndex[2,1] = 'Vendor+cvenfab+cvencolr+fabric+color'
  laIndex[2,2] = 'VenMat'
  laIndex[3,1] = 'Fabric+Color+Vendor'
  laIndex[3,2] = 'Item'
  =gfCrtTmp(lcTmpDet,@laFileStru,@laIndex)
  IF USED(lcTmpDet)
    USE IN (lcTmpDet)
    USE (gcWorkDir+lcTmpDet) IN 0 ORDER MatCol EXCL
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvFabBet
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Showes range screen for fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabBet()
*!*************************************************************
FUNCTION lfvFabBet


lcFabObj = VARREAD()
lcFab    = &lcFabObj
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC

*!**************************************************************************
*! Name      : lpSuppHanl
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : To check for Order status in Order to Enable/Disable the 2
*!             Option added to the Selection grid.
*!**************************************************************************
*! Called from : OG when function
*!**************************************************************************
*! Example   : DO lpChkStat
*!**************************************************************************

PROCEDURE lpSuppHanl
PRIVATE llEmpVen
llEmpVen = EMPTY(lcRpVendor)

IF ASCAN(laOGFxFlt,'VENDMATL.CVENFAB') # 0
  *-- Get the position of the vendor in the varaible filter
  lnVendorPo = ASUBSCRIPT(laOGFxFlt, ASCAN(laOGFxFlt,'VENDMATL.CVENFAB'),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnVendorPo] = !llEmpVen    
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
  IF EMPTY(lcRpVendor) AND USED(laOGFxFlt[lnVendorPo,6]) AND RECCOUNT(laOGFxFlt[lnVendorPo,6]) <> 0
    laOGFxFlt[lnVendorPo,6] = ""
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfSRVFab
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
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
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
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
*! Name      : lfCollect
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Collect Data
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
FUNCTION lfCollect

*----Key Indecies :
*-1 MatCol : vendor+fabric+color+cvenfab+cvencolr
*-2 VenMat : vendor+cvenfab+cvencolr+fabric+color

SELECT VENDMATL
IF EMPTY(lcRPVendor)
  lcWhilExp = ''
  SET ORDER TO MatCol
ELSE
  lcWhilExp = ' WHILE vendor+cvenfab+cvencolr+fabric+color = lcRPVendor '
  SET ORDER TO VenMat
  =SEEK(lcRPVendor)
ENDIF
SCAN &lcWhilExp FOR &lcRpExp
  SCATTER MEMVAR MEMO
  IF !SEEK(IIF(EMPTY(lcRPVendor),vendor+fabric+color+cvenfab+cvencolr,vendor+cvenfab+cvencolr+fabric+color),lcTmpDet)
    INSERT INTO (lcTmpDet) FROM MEMVAR
  ENDIF
ENDSCAN
SET ORDER TO VENMAT1 IN VENDMATL

*--Handle the data not to be reprinted
IF .F. AND EMPTY(lcRPVendor)
 SELECT (lcTmpDet)
 SET ORDER TO ITEM
 SCAN 
   lcKey = EVAL(KEY())
   REPLACE Print with '.T.'
   
 ENDSCAN
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetItem
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Get the current fabric color
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
FUNCTION lfGetItem

lcCurrItem = Fabric + Color
RETURN ''

*!*************************************************************
*! Name      : lfCalcOnOrd
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*: Date      : 08/21/02
*! Purpose   : Calculate the OnOrder qty for a specific 
*!           : Item\Color\Vendor 
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
FUNCTION lfCalcOnOrd
PARAMETERS lcKey,lcVen

PRIVATE lnAlias
lnAlias = SELECT()
SELECT POFLN
=SEEK(lcKey)
SUM REST WHILE fabric+color+cmattype+pomat+trancd = lcKey IIF(TRANCD='1',IIF(POFLN.cMatType = 'P',nFabTOTQTY,-nFabTOTQTY),0.000) TO lnOnOrd FOR (POFLN.cMatType = 'P' OR POFLN.cMatType = 'R') ;
     AND VENDOR = lcVen
SELECT (lnAlias)
RETURN lnOnOrd
