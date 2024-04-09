*:--------------------------------------------------------------------------
*: Program file        : MAFXFDYE.PRG
*: Program description : Fix the fabric/FabDye files.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Matrial - (MA)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : B#603576
*:--------------------------------------------------------------------------
*: Calls               : None.
*:--------------------------------------------------------------------------
*: Called From         : - Main System
*:--------------------------------------------------------------------------
*: Passed Parameters   : lcDataDir = on the form  EX: 'W:\aria27\defs\99'
*:--------------------------------------------------------------------------
*: Example             : DO MAFXFDYE.PRG
*:--------------------------------------------------------------------------
*: Modifications       :
*:--------------------------------------------------------------------------
PARAMETER lcDataDir
WAIT WINDOW "Fix FabDye & Fabric Files " NOWAIT
IF !lfvProcced()
  RETURN
ENDIF
=lfvCancel()

*:----------------------------------------------------------------
*: Name       : lfvProcced
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Function to start fix Fabric&FabDye
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvProcced()
*:----------------------------------------------------------------
*:
FUNCTION lfvProcced

*--- Check Existing of GLDIST FILE
IF FILE(lcDataDir+'FABRIC.DBF')
  IF USED('FABRIC')
    USE IN FABRIC
  ENDIF
  USE lcDataDir+'FABRIC.DBF' IN 0 ORDER  FABRIC
ELSE
  RETURN(.F.)  
ENDIF

*--- Check Existing of Link File
IF FILE(lcDataDir+'FabDye.DBF')
  IF USED('FabDye')
    USE IN FabDye
  ENDIF
  IF USED('NewFabDye')
    USE IN NewFabDye
  ENDIF
  USE lcDataDir+'FabDye.DBF' IN 0 ORDER  FabDye
  USE lcDataDir+'FabDye.DBF' IN 0 AGAIN ALIAS 'NewFabDye' ORDER  FabDye
ELSE
   RETURN(.F.) 
ENDIF


*--- Check WAREHOUS File
IF FILE(lcDataDir+'WAREHOUS.DBF')
  IF USED('WAREHOUS')
    USE IN WAREHOUS
  ENDIF
  USE lcDataDir+'WAREHOUS.DBF' IN 0 ORDER  WAREHOUS
ELSE
   RETURN(.F.)
ENDIF

SELECT FABRIC
GOTO TOP
SCAN
  SCAT MEMVAR MEMO
  lcTFabric = Fabric
  lcTColor  = Color
  WAIT WINDOW "Fix FabDye Records For Fabric + Color = "+ lcTFabric +' + '+lcTColor NOWAIT
  SELECT WAREHOUS
  SET ORDER TO
  GOTO TOP
  lcTWarCod = cWareCode
  SELECT FabDye
  IF SEEK(lcTFabric + lcTColor)
    SCAN REST WHILE fabric+color+cwarecode+dyelot=;
                    lcTFabric + lcTColor
      lcTWarCod = cWareCode
      IF !SEEK(lcTFabric + lcTColor + lcTWarCod +SPACE(08),'NewFabDye')
        *--- Add New Record For Fabric + Color + WareHouse Only.
        =lfAddItem(lcTFabric , lcTColor , lcTWarCod,.F.)
      ENDIF
    ENDSCAN
  ELSE
    *--- Add New Record For Fabric + Color + WareHouse Only.
    =lfAddItem(lcTFabric , lcTColor , lcTWarCod,.T.)
  ENDIF
ENDSCAN


*:----------------------------------------------------------------
*: Name       : lfAddItem
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Valid function for Add Item
*:----------------------------------------------------------------
*: Parameters : lcFab,lcClr,lcWare,llAddFaDef
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfAddItem(Fab1,clr1,Ware1,.T.)
*:----------------------------------------------------------------
*:
FUNCTION lfAddItem
PARAMETER lcFab,lcClr,lcWare,llAddFaDef

SELECT FabDye
INSERT INTO FabDye FROM MEMVAR
REPLACE cWareCode WITH IIF(llAddFaDef,lcTWarCod,lcWare),;
        Dyelot    WITH SPACE(10)

*:----------------------------------------------------------------
*: Name       : lfvCancel
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/2000
*: Purpose    : Valid function for Cancel button
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCancel()
*:----------------------------------------------------------------
*:
FUNCTION lfvCancel

USE IN FABRIC
USE IN FabDye
USE IN WAREHOUS
USE IN NewFabDye