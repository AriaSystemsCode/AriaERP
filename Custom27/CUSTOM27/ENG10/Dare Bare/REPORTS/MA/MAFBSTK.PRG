*:****************************************************************
*: Program file  : MAFBSTK.PRG
*: Program desc. : Fabric Stock Report.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : Khalid Mohi El-Din Mohamed
*: Date          : 02/10/2003
*:****************************************************************
*:C#200470,1
*:****************************************************************

CREATE CURSOR (lcTmpFile) (cWareCode C(6), cFabric C(7), cFabDesc C(30),;
                           cColor C(6), cClrDesc C(30), cVendor C(8),;
                           cRollId C(20), nQty N(12,3),lPrint L)
INDEX ON cWareCode + cFabric + cColor + cRollId TAG (lcTmpFile) OF (lcTmpFile)

STORE '' TO lcWareHous, lcFabFile, lcVendFile, lcFabClr, lcFabTyp
STORE .F. TO llUseWarHs, llUseFab, llUseVend,llPrint
STORE ".T." TO lcClrFltr, lcItmTypFl, lcVendFltr

*-- To get the selected ware houses if any. 
lnPosition = ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcWareHous = laOGFxFlt[lnPosition,6]
  llUseWarHs = IIF(!EMPTY(lcWareHous) .AND. USED(lcWareHous) .AND. RECCOUNT(lcWareHous)>0,.T.,.F.)
ENDIF

*-- To get the selected Customer if any.
lnPosition = ASCAN(laOGFxFlt,'FABRIC.FABRIC')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcFabFile  = laOGFxFlt[lnPosition,6]
  llUseFab   = IIF(!EMPTY(lcFabFile) .AND. USED(lcFabFile) .AND. RECCOUNT(lcFabFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Vendor if any.
lnPosition = ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcVendFile = laOGFxFlt[lnPosition,6]
  llUseVend  = IIF(!EMPTY(lcVendFile) .AND. USED(lcVendFile) .AND. RECCOUNT(lcVendFile)>0,.T.,.F.)
  lcVendFltr = IIF(llUseVend, lcVendFltr + " .AND. SEEK(Fabric.Vendor,lcVendFile)",lcVendFltr)
ENDIF
                           
*-- To get the selected colors
lnPosition = ASCAN(laOGFxFlt,'FABRIC.COLOR')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcFabClr  = laOgFxFlt[lnPosition,6]
  lcClrFltr = IIF(!EMPTY(laOgFxFlt[lnPosition,6]),;
                 lcClrFltr + ' AND Fabric.Color $ lcFabClr',;
   															  lcClrFltr)
ENDIF

*-- To get the selected item types if any.
lnPosition = ASCAN(laOGFxFlt,'FABRIC.ITEM_TYPE')
IF lnPosition > 0
  lnPosition = ASUBSCRIPT(laOGFxFlt,lnPosition,1)
  lcFabTyp  = laOgFxFlt[lnPosition,6]
  lcItmTypFl = IIF(!EMPTY(laOgFxFlt[lnPosition,6]),;
                 lcItmTypFl + ' AND Fabric.Item_Type $ lcFabTyp',;
   															  lcItmTypFl)
ENDIF


*-- If the user selects by fabric
IF llUseFab
  =lfSelByFab()
ELSE
 
  *-- If select by ware house
  IF llUseWarHs
    =lfSelByWH()
  ELSE
    *-- If select by item type, vendor or colors
    =lfSelByAll()
  ENDIF
  
ENDIF

SELECT (lcTmpFile)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DO gfDispRe WITH EVAL('lcRpName')

SET DEVICE TO SCREEN

*!**************************************************************************
*! Name      : lfSelByFab
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : If the user selects by fabric
*!**************************************************************************
FUNCTION  lfSelByFab

SELECT(lcFabFile)
LOCATE
SCAN
  lcFabric = Fabric
  IF SEEK(Fabric,'Fabric')
    SELECT Fabric
    SCAN REST WHILE Fabric+Color = lcFabric
      WAIT WINDOW "Fabric/Colour : " + Fabric+Color NOWAIT      
      *-- Check the validity of colors if there any selected colors.
      *-- And Check the validity of Item Type if there any selected item types.
      *-- And Check the validity of vendor if there any selected vendors.
      IF !EVALUATE(lcClrFltr) OR !EVALUATE(lcItmTypFl) OR !EVALUATE(lcVendFltr)
        LOOP
      ENDIF
        
      IF Fabric.lTrkRolls AND SEEK(Fabric.Fabric+Fabric.Color,'Rolls')
        SELECT Rolls
        SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession = ;
                        Fabric.Fabric+Fabric.Color FOR TranCd = '1' AND nQtyBal > 0
          IF llUseWarHs AND !SEEK(cWareCode,lcWareHous)
            LOOP
          ENDIF
                           
          =lfAddRec(Rolls.cWareCode,Rolls.cRollItem,Fabric.Desc,Fabric.Color,;
                    gfCodDes(Fabric.Color,'COLOR'),Fabric.Vendor,Rolls.cRollId,Rolls.nQtyBal)
        ENDSCAN
        
      ELSE
        
        IF SEEK(Fabric.Fabric+Fabric.Color,'FabDye')
          SELECT Fabdye
          SCAN REST WHILE Fabric+Color+cWareCode+Dyelot = Fabric.Fabric+Fabric.Color;
                    FOR   OnHand > 0
            IF llUseWarHs AND !SEEK(cWareCode,lcWareHous)
              LOOP
            ENDIF
    
            =lfAddRec(FabDye.cWareCode,Fabdye.Fabric,Fabric.Desc,Fabdye.Color,;
                      gfCodDes(FabDye.Color,'COLOR'),Fabric.Vendor,"",Fabdye.OnHand)

          ENDSCAN
        ENDIF          
      ENDIF                                 
      
    ENDSCAN    
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfSelByWH
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : If the user selects by Ware House
*!**************************************************************************
FUNCTION lfSelByWH

SET ORDER TO FabDyeW IN FABDYE

SELECT(lcWareHous)
LOCATE
SCAN
  lcWHouse = cWareCode
  
  IF SEEK(lcWHouse,'FabDye')
    SELECT FabDye
    SCAN REST WHILE cWareCode+Fabric+Color+Dyelot = lcWHouse FOR OnHand > 0
                    
      WAIT WINDOW "Fabric/Colour : " + Fabric+Color NOWAIT      
 

      lcFabricCl = Fabric + Color
      IF SEEK(lcFabricCl,'Fabric')

        *-- Check the validity of colors if there any selected colors.
        *-- And Check the validity of Item Type if there any selected item types.
        *-- And Check the validity of vendor if there any selected vendors.
        IF !EVALUATE(lcClrFltr) OR !EVALUATE(lcItmTypFl) OR !EVALUATE(lcVendFltr)
          LOOP
        ENDIF


        IF Fabric.lTrkRolls AND SEEK(Fabric.Fabric+Fabric.Color+lcWHouse,'Rolls')
          SELECT Rolls
          SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession = ;
                          Fabric.Fabric+Fabric.Color+lcWHouse FOR TranCd = '1' AND nQtyBal > 0
                 
            =lfAddRec(Rolls.cWareCode,Rolls.cRollItem,Fabric.Desc,Fabric.Color,;
                      gfCodDes(Fabric.Color,'COLOR'),Fabric.Vendor,Rolls.cRollId,Rolls.nQtyBal)
          ENDSCAN
        ELSE

          =lfAddRec(FabDye.cWareCode,Fabdye.Fabric,Fabric.Desc,Fabdye.Color,;
                    gfCodDes(FabDye.Color,'COLOR'),Fabric.Vendor,"",Fabdye.OnHand)
        ENDIF
      ENDIF    
    ENDSCAN
    
  ENDIF
ENDSCAN

SET ORDER TO FabDye IN FABDYE

*!**************************************************************************
*! Name      : lfSelByAll
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : If the user selects item type, vendor or color
*!**************************************************************************
FUNCTION lfSelByAll

SET ORDER TO FabDyeW IN FABDYE

SELECT FabDye
SCAN 
  WAIT WINDOW "Fabric/Colour : " + Fabric+Color NOWAIT      
  lcFabricCl = Fabric + Color
  lcWHouse   = cWareCode
  
  IF SEEK(lcFabricCl,'Fabric')

    *-- Check the validity of colors if there any selected colors.
    *-- And Check the validity of Item Type if there any selected item types.
    *-- And Check the validity of vendor if there any selected vendors.
    IF !EVALUATE(lcClrFltr) OR !EVALUATE(lcItmTypFl) OR !EVALUATE(lcVendFltr) OR OnHand <= 0
      LOOP
    ENDIF


    IF Fabric.lTrkRolls AND SEEK(Fabric.Fabric+Fabric.Color+lcWHouse,'Rolls')
      SELECT Rolls
      SCAN REST WHILE cRollItem+Color+cWareCode+Dyelot+cRollId+TranCd+cRSession = ;
                      Fabric.Fabric+Fabric.Color+lcWHouse FOR TranCd = '1' AND nQtyBal > 0
                 
        =lfAddRec(Rolls.cWareCode,Rolls.cRollItem,Fabric.Desc,Fabric.Color,;
                  gfCodDes(Fabric.Color,'COLOR'),Fabric.Vendor,Rolls.cRollId,Rolls.nQtyBal)
      ENDSCAN
    ELSE

      =lfAddRec(FabDye.cWareCode,Fabdye.Fabric,Fabric.Desc,Fabdye.Color,;
                gfCodDes(FabDye.Color,'COLOR'),Fabric.Vendor,"",Fabdye.OnHand)
    ENDIF
  ENDIF    
ENDSCAN

SET ORDER TO FabDye IN FABDYE    

*!**************************************************************************
*! Name      : lfAddRec
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : To add a record to the temporary file
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
FUNCTION lfAddRec
PARAMETERS lcWareHs, lcFab, lcFabDesc, lcColor, lcClrDesc, lcVendor,lcRollId,lnQty

llPrn = IIF(SEEK(lcWareHs,lcTmpFile),.F.,.T.)

INSERT INTO (lcTmpFile) (cWareCode,cFabric,cFabDesc,cColor,cClrDesc,cVendor,cRollId,nQty,lPrint);
             VALUES    (lcWareHs,lcFab,lcFabDesc,lcColor,lcClrDesc,lcVendor,lcRollId,lnQty,llPrn)


*!**************************************************************************
*! Name      : lfFabSum
*! Developer : Khalid Mohi El-Din Mohamed
*: Date      : 02/10/2003
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Returns     : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')
  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)

*!**************************************************************************
*! Name      : lfSRVFab
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : control primary fabric browse 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRVFab()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
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
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE

*!**************************************************************************
*! Name      : lfSetVenRp
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : control the vendor browse 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSetVenRp()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
FUNCTION lfSetVenRp
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
IF lcParm = 'S'  && Set code
    GO TOP IN APVENDOR
ENDIF
*-- End of lfSRVFab.

*!**************************************************************************
*! Name      : lfSetWhs
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/10/2003
*! Purpose   : control the ware house browse.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSetWhs()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
FUNCTION lfSetWhs
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
IF lcParm = 'S'  && Set code
  GO TOP IN WareHous
ENDIF