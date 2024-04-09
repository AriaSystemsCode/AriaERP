*:**************************************************************************
*: Program file  : MADATSEN.PRG
*: Program desc. : DATE SENSITIVE MATERIAL REPORT 
*: Date          : 08/30/2001
*: System        : Aria Advantage Series.
*: Module        : MATERIAL (MA)
*: Developer     : Mohamed Shokry (MHM)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:             
*:         Functions  : lfwRepWhen()
*:                    : lfNoRecord()
*:                    : gfModalGen()
*:                    : gfOpenFile()
*:                    : gfBroWWare()
*:                    : FaBrow()
*:                    : lfCreatFil() 
*:                    : lfSRVFab()
*:                    : lfFabSum()
*:                    : lfvFabric()
*:                    : lfFillAray() 
*:                    : lfvWareHos() 
*:                    : lfwOldWare() 
*:                    : lfvSortBy()
*:                    : lfvPrnWhD()
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Notes   : .... 
*:**************************************************************************
*: Example : DO MADATSEN
*:**************************************************************************
*: This Program is due to Enhancement #E301715
*:**************************************************************************
*:Modifications
*:B606809,1 KHM 01/14/2003 Fix the problem of erasing the location when pressing TAB
*:B607029,1 KHM 05/07/2003 Allow the report to print for all locations in case of select
*!B607029,1                by location. And print the location information in case of
*!B607029,1                sort by fabric and print detail location.
*:B119987,1 MMM 10/06/2003 Fixing Bug of Showing the Fabric that have Total Stock=0 .
*:B119987,1 MMM            Fix Bug of collectng wrong data for fabric based on wrong relation.
*:B119987,1 MMM            Fix Bug Of Wrong Stock Value When Select by Location and then by Fabric.
*:B120094,1 MMM 10/19/2003 Fix Bug Of Evaluating the Inventory at Standard Cost while the
*:B120094,1                Costing method is FIFO                                    
*:B123602,1 NNA 08/12/2004 Fix bug that the report in some cases display a negative unit cost
*:**************************************************************************
lcStTime = TIME()       && To store the current time
lcRpExp = lcRpExp + " .AND. IIF(lcRPDomImp = 'D',Fabric.Make , IIF(lcRPDomImp = 'I',!Fabric.Make , .T.))"
lcRpExp = STRTRAN(lcRpExp,"FABRIC.","")
*-- check first of Sort by WareHouse and Empty of Warehouse Code

*B607029,1 KHM 05/07/2003 (Begin) Comment out this condition in order to print for all warehouses.
*IF lcRpSortBy = 'W' AND (EMPTY(lcWareCode) OR !SEEK(lcWareCode,'WareHous'))
*  *-- < WareHouse Cannot be empty , cannot proceed >
*  = gfModalGen('INM42167B00000','DIALOG')    
*  SET DEVICE TO SCREEN	    
*  RETURN
*ENDIF
*B607029,1 KHM 05/07/2003 (End)


*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.
*-- If User changed the Filter in OG [Begin]

IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcfabTmp) AND RECCOUNT(lcfabTmp) > 0
    = lfCreatFil()
  ENDIF
  
  *-- make the fabric File match the criteria selected
  SELECT FABRIC
  SET FILTER TO &lcRpExp
  *-- If print Warehouse Details is set to YES [Begin]
  IF llRpPrnWhD
    SELECT FABDYE
    *-- If Multi Bins is true [Begin]
    IF llMultiBin
      SET RELATION TO Fabric+cWareCode INTO WhsLoc ADDITIVE
    ENDIF
    *-- If Multi Bins is true [End]
  
    *-B119987,1 MMM 10/06/2003 Fix Bug of collectng wrong data for fabric based on wrong relation [Start]
    *SET RELATION TO Fabric INTO Fabric ADDITIVE  
    SET RELATION TO Fabric+Color INTO Fabric ADDITIVE  
    *-B119987,1 MMM [End]
	IF !EMPTY(lcWareCode)
      SET FILTER TO CWARECODE = lcWareCode AND !EOF('Fabric')
    ENDIF  

    *--  FabDye 
    *--       |___
    *--       |   WhsLoc     (If Multi Bins)     
    *--       |
    *--       |___
    *--           Fabric       
    
  ELSE         && Else print Warehouse Details is set to NO
    SELECT Fabric
    IF llMultiBin
      SET RELATION TO FABRIC INTO WhsLoc ADDITIVE
    ENDIF  

    *--  Fabric
    *--       |___
    *--           WhsLoc     (If Multi Bins)     

  ENDIF
  *-- Endif print Warehouse Details is Yes [Begin]
  
  *-- If no records match criteria
  IF lfNoRecord()
    RETURN
  ELSE
    STORE 0 TO lnRpTVlCst,lnRpTSalVl
    
    *B607029,1 KHM 05/07/2003 (Begin) If sort by location and no ware house selected
    *DO lpCollData   && collect the data to the temporary file
    IF lcRpSortBy='W' AND EMPTY(lcWareCode)
      
      *-- If sort by location and no ware house selected.
      =lfSortByLc()
      
    ELSE
    *B607029,1 KHM 05/07/2003 (End)
      DO lpCollData   && collect the data to the temporary file    
    *B607029,1 KHM 05/07/2003 (Begin)
    ENDIF
    *B607029,1 KHM 05/07/2003 (End)

  ENDIF  
  *-- EndIf of no records match criteria  
ENDIF
*-- If User changed the Filter in OG [End]

*-- if end of file (no records match criteria) [Begin]
SELECT (lcfabTmp)
SET FILTER TO !(nReceived = nIssued)
IF lfNoRecord()
  RETURN
ELSE
  lcExpr = 'ABS(nStkVal)<> 0'

  DO gfDispRe WITH EVALUATE('lcRpName') , 'FOR ' + lcExpr 
ENDIF
*-- Endif of (no records match criteria) [End]
****************************************************************************
*************************** *-- End of Report--* ***************************
****************************************************************************

*!**************************************************************************
*! Name      : lfNoRecord
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Dectect if no records match criteria
*!**************************************************************************
*! Called from : PRG
*!**************************************************************************
*! Calls       : gfModalGen()
*!**************************************************************************
*! Example     : = lfNoRecord()
*!**************************************************************************
*
FUNCTION lfNoRecord
GO TOP               && To activate the file
IF EOF()             && if end of file (no records match criteria)
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF
*-- End of lfNoRecord.

*!**************************************************************************
*! Name      : lfwRepWhen
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfCreatFil()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
SELECT MatInvJl
*:B120094,1 MMM 10/19/2003 Fix Bug Of Evaluating the Inventory at Standard Cost while the
*:B120094,1                Costing method is FIFO. [Start]  
*SET ORDER TO MatInvJl DESCENDING
SET ORDER TO MatInvJl ASCENDING
*:B120094,1 MMM [End]
= lfCreatFil()       && to create the temp file 
*-- end of lfwRepWhen.


*!**************************************************************************
*! Name      : lfCreatFil
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : =lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil

*-- If change Sort By from Fabric to Warehouse or vise versa close the temp file first 
IF USED(lcfabTmp)
  USE IN (lcfabTmp)
ENDIF
*-- Var lcFileName to hold the Master File whether it's FABRIC or FABDYE
lcFileName = IIF(lcRpSortBy = 'S','FABRIC','FABDYE')

SELECT MatInvJl
=AFIELDS(laFileStru) 

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'DESC'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 20
laFileStru[lnNewFld,4] = 0
  
lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Ave_Cost'
laFileStru[lnNewFld,2] = 'N'
laFileStru[lnNewFld,3] = 10

*B607029,1 KHM 05/07/2003 (Begin) Changing the decimal to be 3 instead of 2.
*laFileStru[lnNewFld,4] = 2
laFileStru[lnNewFld,4] = 3

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'llWare'
laFileStru[lnNewFld,2] = 'L'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

*B607029,1 KHM 05/07/2003 (End)

CREATE DBF (gcWorkDir+lcfabTmp) FROM ARRAY laFileStru     && create the temp file

=gfOpenFile(gcWorkDir+lcfabTmp,'','EX')   && to open the Temp File Exclusively

*B607029,1 KHM 05/07/2003 (Begin) If sort by location and not selection a location code 
*B607029,1                index on the cwarecode
*INDEX ON cFabric+cColor+cWareCode TAG (lcfabTmp) OF (lcfabTmp)

IF lcRpSortBy='W' AND EMPTY(lcWareCode)
  INDEX ON cWareCode+cFabric+cColor TAG (lcfabTmp) OF (lcfabTmp)
ELSE
  INDEX ON cFabric+cColor+cWareCode TAG (lcfabTmp) OF (lcfabTmp)
ENDIF
*B607029,1 KHM 05/07/2003 (End)


*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfvFabric
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
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
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!*************************************************************
*! Name      : lfSRVFab
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
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
    =gfOpenFile(gcDataDir+'Fabric',gcDataDir+'Fabric','SH',@lcTFabFile,.T.)
    *USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO (lcTFabFilew)
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN (lcTFabFile)
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.
*!*************************************************************
*! Name      : lfFabSum
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
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

SELECT (lcTFabFile)
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.


*!**************************************************************************
*! Name      : lfFillAray
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Fill Sort By Array
*!**************************************************************************
*! Example   : = lfArrDummy()
*!**************************************************************************
*
FUNCTION lfFillAray
DIMENSION laSortDesc[2,1] , laSortVal[2,1]
laSortDesc[1,1] = 'Fabric'
laSortDesc[2,1] = 'Location'
laSortVal[1,1]  = 'S'
laSortVal[2,1]  = 'W'
*-- End of lfFillAray.

*!**************************************************************************
*! Name      : lfvWareHos
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Validate Warehouse Code
*!**************************************************************************
*! Example   : = lfvWareHos()
*!**************************************************************************
*
FUNCTION lfvWareHos
PRIVATE lcWareHous , lcTag

lcWareHous = VARREAD()
lcTag      = ORDER('WAREHOUS')

*SET ORDER TO WAREHOUS IN WAREHOUS

*B606809,1 KHM 01/14/2003 (Begin) Commenting the following IF ELSE.
*IF LASTKEY() = 13 AND !MDOWN()
*B606809,1 KHM 01/14/2003 (End)

  *B607029,1 KHM 05/07/2003 (Begin) Add the check if the ware house is not empty.
  *IF SEEK(&lcWareHous.,'WAREHOUS') 
  IF !EMPTY(lcWareCode)
  *B607029,1 KHM 05/07/2003 (End)
*-B119987,1 MMM 10/06/2003 Fixing Bug of Showing the Fabric that have Total Stock=0 [start]
  select (lcfabtmp)

  INDEX ON cFabric+cColor+cWareCode TAG (lcfabTmp) OF (lcfabTmp)
*-B119987,1 MMM [End]
    IF SEEK(&lcWareHous.,'WAREHOUS') 
      &lcWareHous = WAREHOUS.cWareCode
    ELSE
      &lcWareHous = gfBroWWare(.T.)
    ENDIF
  ENDIF  
*B606809,1 KHM 01/14/2003 (Begin)
*ELSE
*  &lcWareHous = lcOldWare
*ENDIF
*B606809,1 KHM 01/14/2003 (End)
SET ORDER TO &lcTag IN WAREHOUS

*-- End of lfvWareHos.


*!**************************************************************************
*! Name      : lfwOldWare
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : To get the old value of warehouse
*!**************************************************************************
*! Example   : = lfwOldWare()
*!**************************************************************************
*
FUNCTION lfwOldWare
lcOldWare = EVALUATE(SYS(18))
*-- End of lfwOldWare.


*!**************************************************************************
*! Name      : lfvSortBy
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : valid sort by function
*!**************************************************************************
*! Example   : =lfvSortBy()
*!**************************************************************************
*
FUNCTION lfvSortBy
*-B119987,1 MMM 10/06/2003 Fix Bug Of Wrong Stock Value When Select by Location and then by Fabric [Start]
*llRpPrnWhD = IIF(lcRpSortBy='W',.T.,llOldPrWhD)
llRpPrnWhD = IIF(lcRpSortBy='W',.T.,.F.)
*-B119987,1 MMM  [End]
CLEAR READ
*-- End of lfvSortBy.

*!**************************************************************************
*! Name      : lfvPrnWhD
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : valid print warehouse details
*!**************************************************************************
*! Example   : =lfvPrnWhD()
*!**************************************************************************
*
FUNCTION lfvPrnWhD
llOldPrWhD = llRpPrnWhD   && Save current print ware house details in another 
*-- End of lfvPrnWhD.


*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mohamed Shokry (MHM)
*! Date      : 08/30/2001
*! Purpose   : Collect the data from MatInvJl file.
*!**************************************************************************
*! Called from : FRX
*!**************************************************************************
*! Example   : =lpCollData()
*!**************************************************************************
*!
PROCEDURE lpCollData
IF lcRpSortBy='W' AND EMPTY(lcWareCode)
  lcSeekExp = [cWareCode+cFabric+cColor]
ELSE
  lcSeekExp = [cFabric+cColor+cWareCode]
ENDIF

lcWareCode = IIF(lcRpSortBy = 'W',lcWareCode,'')

SELECT (lcFileName)
SCAN
  WAIT WINDOW 'Collecting Data for ' + Fabric NOWAIT
  *lcFabric   = IIF(lcFileName = 'FABDYE',FABRIC.FABRIC,FABRIC)
  *lcColor    = IIF(lcFileName = 'FABDYE',FABRIC.COLOR,COLOR)
  lcFabric   = FABRIC
  lcColor    = COLOR
  SELECT MatInvJl
  IF SEEK(lcFabric+lcColor+lcWareCode)

    SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession= lcFabric+lcColor;
              FOR dTranDate <= ldRpDate AND ;
              IIF(!EMPTY(lcWareCode),cWareCode = lcWareCode,.T.)

      WAIT WINDOW 'Collecting Data for ' + cFabric NOWAIT
      SCATTER MEMVAR
      =SEEK(lcFabric+lcColor,'Fabric')
    
      SELECT(lcfabTmp)

      IF !SEEK(lcFabric+lcColor+lcWareCode)
 
        INSERT INTO (lcfabTmp) FROM MEMVAR
        lnTotStk = nReceived - nIssued
        *B123602,1 NNA 08/12/2004 (Begin) Get the Absolute Value for the Ave_Cost (Unit cost on the Report)
        *REPLACE Desc     WITH Fabric.Desc   ,;
                Ave_Cost WITH IIF(lnTotStk<>0,nStkVal/lnTotStk,0)
        REPLACE Desc     WITH Fabric.Desc   ,;
                Ave_Cost WITH IIF(lnTotStk<>0,ABS(nStkVal/lnTotStk),0)
       *B123602,1 NNA (End)
 
        *B607029,1 KHM 05/07/2003 (Begin) If sort by fabric.
        REPLACE cWareCode WITH lcWareCode
        *-- Replace nStkVal with if Ave_Cost = 0 or lnTotStk
        IF lnTotStk = 0 OR Ave_Cost = 0
          REPLACE nStkVal WITH 0
        ENDIF
        *B607029,1 KHM 05/07/2003 (End)
      ELSE
        lnTotStk = (nReceived + m.nReceived)- (nIssued + m.nIssued )
        *B123602,1 NNA 08/12/2004 (Begin) Get the Absolute Value for the Ave_Cost (Unit cost on the Report)
        *REPLACE  nreceived  WITH nReceived + m.nReceived ,;
                 nissued    WITH nIssued + m.nIssued ,;
                 nStkVal    WITH nStkVal + m.nStkVal,;
                 Ave_Cost   WITH IIF(lnTotStk<>0,nStkVal /lnTotStk,0)
        REPLACE  nreceived  WITH nReceived + m.nReceived ,;
                 nissued    WITH nIssued + m.nIssued ,;
                 nStkVal    WITH nStkVal + m.nStkVal,;
                 Ave_Cost   WITH IIF(lnTotStk<>0,ABS(nStkVal /lnTotStk),0)
       *B123602,1 NNA (End)

        *B607029,1 KHM 05/07/2003 (Begin) Replace nStkVal with if Ave_Cost = 0 or lnTotStk
        IF lnTotStk = 0 OR Ave_Cost = 0
          REPLACE nStkVal WITH 0
        ENDIF
        *B607029,1 KHM 05/07/2003 (End)

      ENDIF
      
      *B607029,1 KHM 05/07/2003 (Begin) If sort by fabric and print detail location
      IF lcRpSortBy = 'S'
        IF llRpPrnWhD
          IF !SEEK(lcFabric+lcColor+MatInvJl.cWareCode)
            INSERT INTO (lcfabTmp) FROM MEMVAR
            lnTotStk = nReceived - nIssued
            REPLACE Desc     WITH Fabric.Desc   ,;
                    Ave_Cost WITH IIF(lnTotStk<>0,nStkVal/lnTotStk,0),;
                    llWare   WITH .T.
            IF lnTotStk = 0 OR Ave_Cost = 0
              REPLACE nStkVal WITH 0
            ENDIF
          ELSE
            lnTotStk = (nReceived + m.nReceived)- (nIssued + m.nIssued )
            REPLACE  nreceived  WITH nReceived + m.nReceived ,;
                     nissued    WITH nIssued + m.nIssued ,;
                     nStkVal    WITH nStkVal + m.nStkVal,;
                     Ave_Cost   WITH IIF(lnTotStk<>0,nStkVal /lnTotStk,0)
            IF lnTotStk = 0 OR Ave_Cost = 0
              REPLACE nStkVal WITH 0
            ENDIF
          ENDIF
        
        ENDIF
      ENDIF
      *B607029,1 KHM 05/07/2003 (End)
        
    ENDSCAN
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfSortByLc
*: Developer : Khalid Mohi El-Din (KHM)
*! Date      : 05/07/2003
*! Purpose   : Called if sort by location and no ware house selected.
*!**************************************************************************
*! Example   : =lfSortByLc()
*!**************************************************************************
*!B607029,1 KHM 05/07/2003 
*!**************************************************************************
FUNCTION lfSortByLc
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT FabDye
SET RELATION TO

SELECT Fabric
SCAN
  WAIT WINDOW 'Collecting Data for ' + Fabric NOWAIT
  
  IF SEEK(Fabric.Fabric+Fabric.Color,'FabDye')
    SELECT FabDye
    SCAN REST WHILE Fabric+Color+cWareCode+Dyelot= Fabric.Fabric+Fabric.Color    
  
      IF SEEK(FabDye.Fabric+FabDye.Color+FabDye.cWareCode,'MatInvJl')
        SELECT MatInvJl
        SCAN REST WHILE cFabric+cColor+cWareCode+cDyelot+cRsession+cIsession= ;
                        FabDye.Fabric+FabDye.Color+FabDye.cWareCode ;
                  FOR dTranDate <= ldRpDate

          WAIT WINDOW 'Collecting Data for ' + cFabric NOWAIT
          SCATTER MEMVAR
    
          SELECT(lcfabTmp)
          IF !SEEK(FabDye.cWareCode+FabDye.Fabric+FabDye.Color)
            INSERT INTO (lcfabTmp) FROM MEMVAR
            lnTotStk = nReceived - nIssued
            REPLACE Desc     WITH Fabric.Desc   ,;
                    Ave_Cost WITH IIF(lnTotStk<>0,nStkVal/lnTotStk,0)
            
            IF lnTotStk = 0 OR Ave_Cost = 0
              REPLACE nStkVal WITH 0
            ENDIF

          ELSE
            lnTotStk = (nReceived + m.nReceived)- (nIssued + m.nIssued )
            REPLACE  nreceived  WITH nReceived + m.nReceived ,;
                     nissued    WITH nIssued + m.nIssued ,;
                     nStkVal    WITH nStkVal + m.nStkVal,;
                     Ave_Cost   WITH IIF(lnTotStk<>0,nStkVal /lnTotStk,0)
            IF lnTotStk = 0 OR Ave_Cost = 0
              REPLACE nStkVal WITH 0
            ENDIF

          ENDIF
        ENDSCAN
      ENDIF    
    ENDSCAN
  ENDIF  
ENDSCAN
SELECT(lnAlias)