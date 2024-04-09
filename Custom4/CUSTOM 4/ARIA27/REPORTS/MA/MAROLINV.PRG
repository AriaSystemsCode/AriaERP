*!************************************************************************************************
*: Program file  : MAROLINV.PRG
*: Program desc. : MATERIAL INVENTORY REPORTS BY ROLL.
*:         System: ARIA APPAREL SERIES
*:         Module: MA
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:           Date: 02/22/2000
*!************************************************************************************************
*! Refer to (C101767)
*!************************************************************************************************
*!Modifications : 
*!B803148,1 ADEL 03/23/2000 - Just get rolls having Trancd = 1 to avoid doublicating recodrs having any tranactions.
*!B803148,1                 - Change the mode of 'Date' field in the header of the 'MAROLINV.FRX' to be 'Transperant'
*!************************************************************************************************

*----Variables used in OG
*-- llRpOnHand  && Only Onhand Qty?
*-- llRpRolDet  && Print roll detail?
*-- llRpDyeDet  && Print dyelot detail?
*-- lcRpTitle   && Title.
*-- lcRpSort    && Sort By.

*--Have we got data matching the user's selection criteria.
SELECT FABRIC
LOCATE FOR &lcRpExp
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*--Collect data only when the filter changes.
IF llOgFltCh
  IF USED(lcFabTemp) AND RECCOUNT(lcFabTemp) > 0
    SELECT (lcFabTemp)
    ZAP
  ENDIF 
  =lfCollect()
ENDIF
SELECT (lcFabTemp)
GO TOP
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*-- Start printing
DO gfDispRe WITH EVAL('lcRepName')
SET DEVICE TO SCREEN
RETURN
*--End of program MAROLINV

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/22/2000
*! Purpose   : sum a specific field for the current fabric in fabric file
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
*! Name      : lfSRVFab
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/22/2000
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
*! Name      : lfWhenFunc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/22/2000
*! Purpose   : Valid WHNE fulction.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfWhenFunc

*-- Create temp file
CREATE TABLE (gcWorkDir+lcFabTemp) (Fabric C(7), Color C(6), Vendor C(8), Desc C(20), Width C(6),Loc C(6), OnHand N(12,3),;
                                    Uom C(3), Cost N(10,3), Value N(12,3), RollID C(20), DyeLot C(10), ItemType C(6))
INDEX ON Vendor    TAG Vendor   ADDITIVE
INDEX ON Loc       TAG Location ADDITIVE
INDEX ON ItemType  TAG ItemType ADDITIVE
INDEX ON Fabric    TAG Fabric   ADDITIVE
*--Get the company address
=gfOpenFile(gcSysHome+'syccomp',gcSysHome+'Ccomp_id','SH')
=SEEK(gcAct_Comp,'SycComp')


*!*************************************************************
*! Name      : lfvSort
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/22/2000
*! Purpose   : Valid sort by option.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
FUNCTION lfvSort

lcIndName = IIF(lcRpSort = 'V','Vendor',IIF(lcRpSort = 'C','Fabric',IIF(lcRpSort = 'L','Location','ItemType')))
SELECT (lcFabTemp)
SET ORDER TO TAG lcIndName

*!*************************************************************
*! Name      : lfCollect
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 02/22/2000
*! Purpose   : Collect data.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
FUNCTION lfCollect

SELECT FABRIC
SCAN WHILE INKEY()<>32 FOR &lcRpExp
  WAIT WINDOW 'Collecting data..please wait..Space bar to abort!  Fabric\color '+fabric+'\'+color NOWAIT
  *-- If the fabric\color does not exist in FABDYE, this means it has no stock so skip it.
  IF SEEK(FABRIC+COLOR,'FABDYE')
    SELECT FABDYE
    *-- If the user does not want to print dyelot detail then get the warehouse record (holding total dyelots) otherwise
    *-- get all dyelots records.
    SCAN REST WHILE fabric+color+cwarecode+dyelot = Fabric.Fabric+Fabric.Color FOR IIF(llRpDyeDet AND Fabric.cDye_Flg = 'Y',!EMPTY(Dyelot),EMPTY(Dyelot))
      *--If the user wants to print roll detial and fabric/color/warehouse/dyelot (whether dyelot is empty or not) has rolls, get them.
      IF llRpRolDet AND SEEK(Fabric+Color+cWareCode+Dyelot,'ROLLS')
        lcKey = Fabric+Color+cWareCode+Dyelot
        SELECT ROLLS
        *B803148,1 (Begin) Just get rolls having Trancd = 1
        *SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcKey FOR IIF(llRpOnHand,nQtyBal <>0,.T.)
        SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcKey FOR ( IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
        *B803148,1 (End)
         INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,Loc             , OnHand ,Uom          , Cost         , Value                , RollID            , DyeLot , ItemType );
                          VALUES (Fabric.fabric,Fabric.Color,Fabric.Vendor,Fabric.Desc,Fabric.Width,FabDye.cWarecode,Rolls.nQtyBal ,Fabric.UomUse,Fabric.CostUse,Rolls.nQtyBal*Fabric.CostUse,Rolls.cRollID,Rolls.DyeLot,Fabric.Item_Type)
                          
        ENDSCAN
      ELSE
        *-- Insert a non-zero onhand records if the user selcets 'Only OnHand Qty' with 'Y'.
        IF !llRpOnHand OR FABDYE.OnHand <>0 
          *--Fabric+Color+cWareCode+Dyelot doesn't have any rolls, get the warehouse record.
           INSERT INTO (lcFabTemp) (Fabric       , Color      , Vendor      , Desc      , Width      ,Loc             , OnHand       ,Uom          , Cost         , Value                      , RollID , DyeLot      , ItemType );
                            VALUES (Fabric.fabric,Fabric.Color,Fabric.Vendor,Fabric.Desc,Fabric.Width,FabDye.cWarecode,FabDye.OnHand ,Fabric.UomUse,Fabric.CostUse,FabDye.OnHand*Fabric.CostUse,SPACE(20),FabDye.DyeLot,Fabric.Item_Type)
        ENDIF
      ENDIF  
    ENDSCAN
  ENDIF  
ENDSCAN