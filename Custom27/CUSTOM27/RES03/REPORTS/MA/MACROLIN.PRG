*!************************************************************************************************
*: Program file  : MACROLIN.PRG
*: Program desc. : CUSTOM MATERIAL INVENTORY REPORTS BY ROLL.
*:         System: ARIA APPAREL SERIES
*:         Module: MA
*:      Developer: NADER NABIL (NNA)
*:           Date: 03/29/2005
*!************************************************************************************************
*! Refer to (C126216)
*!************************************************************************************************
*!Modifications : 
*!************************************************************************************************
*!* Variables used in OG
*!* llRpOnHand  && Only Onhand Qty?
*!* llRpRolDet  && Print roll detail?
*!* llRpDyeDet  && Print dyelot detail?
*!* lcRpTitle   && Title.
*!* lcRpSort    && Sort By.
*!* Have we got data matching the user's selection criteria.
SELECT FABRIC
LOCATE FOR &lcRpExp
IF EOF()
  *!* Text 'NO RECORDS SELECTED FOR REPORT!'
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
  *!* Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
*!* Start printing
lcSbTotExp ='S_Total For ' +IIF(lcRpSort = 'V','Vendor'  ,;
                              IIF(lcRpSort = 'C','Fabric'  ,;
                              IIF(lcRpSort = 'L','Location',;
                              IIF(lcRpSort = 'T','Item Type','Pur. Group')))) + ' : '
DO gfDispRe WITH EVAL('lcRepName')
SET DEVICE TO SCREEN
RETURN
*!* End of Report MACROLIN.
*!*************************************************************
*! Name      : lfFabSum
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
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
*!C126216,1
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

*!* end of lfFabSum.
*!*************************************************************
*! Name      : lfSRVFab
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     :  Functions  : gfModalGen
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
*!C126216,1
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *!* open this file in another alias to set order to primary fabric
    *!* unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*!* End of lfSRVFab.
*!*************************************************************
*! Name      : lfWhenFunc
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
*! Purpose   : Valid WHNE fulction.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*!C126216,1
FUNCTION lfWhenFunc

*-- Create temp file
CREATE TABLE (gcWorkDir+lcFabTemp) (Fabric C(7), Color C(6), Vendor C(8),VenName C(30),;
       Desc C(20), Width C(6),Loc C(6), OnHand N(12,3),Uom C(3), Cost N(10,3),Value N(12,3),;
       RollID C(20), DyeLot C(10),ItemType C(6),CpurCode C(6))
INDEX ON Vendor    TAG Vendor   ADDITIVE
INDEX ON Loc       TAG Location ADDITIVE
INDEX ON ItemType  TAG ItemType ADDITIVE
INDEX ON Fabric    TAG Fabric   ADDITIVE
INDEX ON CPURCODE  TAG CPURCODE ADDITIVE
*--Get the company address
=gfOpenFile(gcSysHome+'syccomp',gcSysHome+'Ccomp_id','SH')
=SEEK(gcAct_Comp,'SycComp')
*!*************************************************************
*! Name      : lfvSort
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
*! Purpose   : Valid sort by option.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
*!C126216,1
FUNCTION lfvSort
lcIndName = IIF(lcRpSort = 'V','Vendor'  ,;
            IIF(lcRpSort = 'C','Fabric'  ,;
            IIF(lcRpSort = 'L','Location',;
            IIF(lcRpSort = 'T','ItemType','CpurCode'))))
SELECT (lcFabTemp)
SET ORDER TO TAG lcIndName

*!*************************************************************
*! Name      : lfCollect
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
*! Purpose   : Collect data.
*!*************************************************************
*! Called from : Sort By
*!*************************************************************
*!C126216,1
FUNCTION lfCollect
PRIVATE lcVendor,lnCost,lcVenName
STORE '' TO lcVendor,lcVenName
STORE 0 TO lnCost
SELECT FABRIC
SCAN WHILE INKEY()<>32 FOR &lcRpExp
  WAIT WINDOW 'Collecting data..please wait..Space bar to abort!  Fabric\color '+fabric+'\'+color NOWAIT
  *!* If the fabric\color does not exist in FABDYE, this means it has no stock so skip it.
  IF SEEK(FABRIC+COLOR,'FABDYE')
    SELECT FABDYE
    *!* If the user does not want to print dyelot detail then get the warehouse record (holding total dyelots) otherwise
    *!* get all dyelots records.
    SCAN REST WHILE fabric+color+cwarecode+dyelot = Fabric.Fabric+Fabric.Color FOR ;
         IIF(llRpDyeDet AND Fabric.cDye_Flg = 'Y',!EMPTY(Dyelot),EMPTY(Dyelot))
      *!* If the user wants to print roll detial and fabric/color/warehouse/dyelot (whether dyelot is empty or not) has rolls, get them.
      lcKey = FABDYE.Fabric+FABDYE.Color+FABDYE.cWareCode+FABDYE.Dyelot
      IF llRpRolDet AND SEEK(lcKey,'ROLLS')
        SELECT ROLLS
        *!* Just get rolls having Trancd = 1
        SCAN REST WHILE crollitem+color+cwarecode+dyelot+crollid+trancd+crsession = lcKey FOR ;
             (IIF(llRpOnHand,nQtyBal <>0,.T.)  AND TRANCD ='1' )
          IF SEEK(lcKey+crsession,'MATINVJL')
            lnCost = MATINVJL.NunitCost
            IF SEEK('P'+MatInvJl.Ctran,'POFHDR')
              lcVendor = PofHdr.Vendor
            ELSE
              lcVendor = Fabric.Vendor
            ENDIF
          ELSE
            lcVendor = Fabric.Vendor
            lnCost   = 0
          ENDIF
          IF SEEK(lcVendor,'APVENDOR')
            lcVenName = APVENDOR.cVenComp
          ENDIF
          INSERT INTO (lcFabTemp) (Fabric,Color,Vendor,VenName,Desc,Width,Loc,OnHand,Uom,Cost,Value,;
                 RollID,DyeLot,ItemType,cPurCode);
          VALUES (Fabric.fabric,Fabric.Color,lcVendor,lcVenName,;
                 Fabric.Desc,Fabric.Width,FabDye.cWarecode,Rolls.nQtyBal,Fabric.UomUse,;
                 IIF(lnCost>0,lnCost,Fabric.CostUse),;
                 IIF(lnCost>0,Rolls.nQtyBal*lnCost,Rolls.nQtyBal*Fabric.CostUse),;
                 Rolls.cRollID,Rolls.DyeLot,Fabric.Item_Type,Fabric.cPurCode)
        ENDSCAN
      ELSE
        *!* Insert a non-zero onhand records if the user selcets 'Only OnHand Qty' with 'Y'.
        IF !llRpOnHand OR FABDYE.OnHand <>0 
          *!* Fabric+Color+cWareCode+Dyelot doesn't have any rolls, get the warehouse record.
          IF SEEK(Fabric.Vendor,'APVENDOR')
            lcVenName = APVENDOR.cVenComp
          ENDIF
          INSERT INTO (lcFabTemp) (Fabric,Color,Vendor,VenName,Desc,Width,Loc,OnHand,Uom,Cost,Value,;
                 RollID,DyeLot,ItemType,cPurCode);
          VALUES (Fabric.fabric,Fabric.Color,Fabric.Vendor,lcVenName,Fabric.Desc,Fabric.Width,;
                 FabDye.cWarecode,FabDye.OnHand,Fabric.UomUse,Fabric.Nfave_Cost,;
                 FabDye.OnHand*Fabric.Nfave_Cost,SPACE(20),FabDye.DyeLot,Fabric.Item_Type,;
                 Fabric.cPurCode)
        ENDIF
      ENDIF  
    ENDSCAN
  ENDIF  
ENDSCAN
*--End of Funstion lfCollect.

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : NADER NABIL (NNA)
*! Date      : 03/29/2005
*! Purpose   : Get the Item Type Code or Purchase Group or Vendor Name
*!*************************************************************
*! Called from : Report FRX
*!*************************************************************
*!C126216,1
FUNCTION lfGetCodes
STORE '' TO lcCodExp
lcCodExp = ALLTRIM(IIF(lcRpSort = 'V',Vendor,;
                      IIF(lcRpSort = 'C',Fabric,IIF(lcRpSort = 'L',Loc,;
                      IIF(lcRpSort = 'T',ItemType,CpurCode)))))
lcCodExp = lcCodExp+'  '+ IIF(lcRpSort ='T',SUBSTR(gfCodDes(ITEMTYPE,'ITEM_TYPE'),1,20),;
                      IIF(lcRpSort ='P',SUBSTR(gfCodDes(CPURCODE,'CPURCODE'),1,30) ,;
                      IIF(lcRpSort ='V',VENNAME,'')))
RETURN lcCodExp
*--End of Funstion lfGetCodes.
